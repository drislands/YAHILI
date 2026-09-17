{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Language

import Control.Monad.State (StateT, MonadIO (liftIO), MonadState (get, put), MonadTrans (lift), modify')
import Control.Monad.Except (ExceptT, MonadError (throwError, catchError))
import Prelude hiding (error, lookup)
import qualified Data.Map as Map


data EvalSignal = 
    SigError EvalError |
    SigReturn Value
type Evaluating m a = StateT Variables (ExceptT EvalSignal m) a

-- Expression evaluation first.
evaluateExpression :: MonadIO m => Expression -> Evaluating m Value
evaluateExpression = \case
    -- Literals
    LString  s -> pure $ VString  s
    LBoolean b -> pure $ VBoolean b
    LNumber  d -> pure $ VNumber  d
    LNil       -> pure $ VNil
    -- Expressions!
    Binary left op right  -> evaluateBinary left op right
    Grouping e            -> evaluateExpression e
    Unary op right        -> evaluateUnary op right
    Assignment t val      -> evaluateAssignment t val
    Identifier t          -> evaluateIdentifier t
    -- More complex stuff, like control flow!
    Logical left op right -> evaluateLogical left op right
    Call callee t args    -> evaluateCall callee t args

-- Individual evaluation functions

evaluateBinary :: MonadIO m => Expression -> Token -> Expression -> Evaluating m Value
evaluateBinary left op right = case lookupOp (getTokenType op) of
    Just f -> f op left right
    Nothing -> error op "Invalid binary operator."
  where
    lookupOp :: MonadIO m => TokenType -> Maybe (Token -> Expression -> Expression -> Evaluating m Value)
    lookupOp = \case
        -- Term
        Lx_Plus         -> Just evaluatePlus
        Lx_Minus        -> Just evaluateMinus
        -- Factor
        Lx_Star         -> Just evaluateStar
        Lx_Slash        -> Just evaluateSlash
        -- Equality
        Lx_BangEqual    -> Just evaluateInequality
        Lx_EqualEqual   -> Just evaluateEquality
        -- Comparison
        Lx_Less         -> Just evaluateLess
        Lx_LessEqual    -> Just evaluateLessEqual
        Lx_Greater      -> Just evaluateGreater
        Lx_GreaterEqual -> Just evaluateGreaterEqual
        _ -> Nothing

evaluateUnary :: MonadIO m => Token -> Expression -> Evaluating m Value
evaluateUnary op right = do
    case getTokenType op of
        Lx_Minus -> do
            r <- evaluateExpression right
            case r of
                VNumber d -> pure $ VNumber (0 - d)
                _ -> error op "Operand must be a number."
        Lx_Bang  -> do
            r <- evaluateExpression right
            pure $ VBoolean ((not . truthy) r)
        _ -> error op "Invalid unary operator."

evaluateAssignment :: MonadIO m => Token -> Expression -> Evaluating m Value
evaluateAssignment t val = do
    let name = getLexeme t
    vars <- get
    val' <- evaluateExpression val
    case assign name val' vars of
        Just vars' -> do
            put vars'
            pure val'
        Nothing -> error t
            ("Undefined variable '" <> name <> "'.")

evaluateIdentifier :: MonadIO m => Token -> Evaluating m Value
evaluateIdentifier t = do
    let name = getLexeme t
    vars <- get
    case lookup name vars of
        Just x -> pure x
        Nothing -> error t 
            ("Undefined variable '" <> name <> "'.")

evaluateLogical :: MonadIO m => Expression -> Token -> Expression -> Evaluating m Value
evaluateLogical left op right = do
    left' <- evaluateExpression left
    if (getTokenType op == Lx_Or) == truthy left' then
        pure left'
    else evaluateExpression right

evaluateCall :: MonadIO m => Expression -> Token -> [Expression] -> Evaluating m Value
evaluateCall callee t args = do
    callee' <- evaluateExpression callee
    case callee' of
        VCallable arity callable -> do
            if arity /= length args then error t ("Expected " <> 
                show arity <> " arguments but got " <> 
                show (length args) <> ".")
            else do
                args' <- mapM evaluateExpression args
                case callable of
                    UserDefined declaration functionLocals -> do
                        (callerLocals,g) <- get
                        let FunctionDeclaration 
                             { funName   = name
                             , funParams = params
                             , funBody   = body
                             } = declaration
                            targetName = case callee of
                                Identifier tok -> getLexeme tok
                                _              -> name
                        put (Map.fromList (zip params args') : functionLocals,g)
                        (val,(functionLocals',g')) <- runClosure body
                        let updatedCallable = VCallable arity (UserDefined declaration functionLocals')
                        let finalVars = case assign targetName updatedCallable (callerLocals,g') of
                             Just vars -> vars   
                             Nothing   -> (callerLocals,g')
                        put finalVars
                        pure val
                    NativeFunction iofunc -> do
                        result <- lift $ liftIO (iofunc args')
                        case result of
                            Left er -> throwError $ SigError er
                            Right val -> pure val
        _ -> error t 
            ("Can only call functions and classes.")
  where
    handleSignal :: MonadIO m => EvalSignal -> Evaluating m Value
    handleSignal = \case
        SigError er -> throwError $ SigError er
        SigReturn val -> pure val
    runClosure :: MonadIO m => Statement -> Evaluating m (Value,Variables)
    runClosure body = do
        val <- (evaluateStatement body >> pure VNil) `catchError` handleSignal
        (locals,g) <- get
        case locals of
            _ : rest -> pure (val,(rest,g))
            rest     -> pure (val,(rest,g))


-- Binary functions
evaluateBinaryValues :: MonadIO m => ((Value,Value) -> Evaluating m Value) -> Expression -> Expression -> Evaluating m Value
evaluateBinaryValues f left right = do
    l <- evaluateExpression left
    r <- evaluateExpression right
    f (l,r)

-- Math (and string concatenation)
evaluatePlus :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluatePlus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1+v2)
    (VString s1,VString s2) -> pure $ VString (s1<>s2)
    _ -> error op "Operands must be two numbers or two strings."

evaluateMinus :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateMinus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1-v2)
    _ -> error op "Operands must be two numbers."

evaluateStar :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateStar op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1*v2)
    _ -> error op "Operands must be two numbers."

evaluateSlash :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateSlash op = evaluateBinaryValues $ \case
    (VNumber _,VNumber 0)  -> error op "Cannot divide by zero."
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1/v2)
    _ -> error op "Operands must be two numbers."

-- Boolean logic
evaluateEquality :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateEquality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l == r)

evaluateInequality :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateInequality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l /= r)

-- Number comparison
evaluateLess :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateLess op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 < v2)
    _ -> error op "Operands must be two numbers."

evaluateLessEqual :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateLessEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 <= v2)
    _ -> error op "Operands must be two numbers."

evaluateGreater :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateGreater op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 > v2)
    _ -> error op "Operands must be two numbers."

evaluateGreaterEqual :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateGreaterEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 >= v2)
    _ -> error op "Operands must be two numbers."


-- Statement evaluation next.
evaluateStatement :: MonadIO m => Statement -> Evaluating m ()
evaluateStatement = \case
    VarDeclaration name expr -> do
        val <- evaluateExpression expr
        vars <- get
        put $ define name val vars
    PrintStatement      expr -> do
        val <- evaluateExpression expr
        liftIO $ putStrLn $ show val
    ExpressionStatement expr -> 
        evaluateExpression expr >> pure ()
    Block statements -> do
        modify' addScope
        mapM_ evaluateStatement statements
        modify' dropScope
    IfStatement condition thenBranch elseBranch -> do
        val <- evaluateExpression condition
        if truthy val then evaluateStatement thenBranch
        else case elseBranch of
            Just elseBranch' -> evaluateStatement elseBranch'
            Nothing -> pure ()
    WhileStatement condition body -> evaluateWhile condition body
    FunDeclaration declaration    -> evaluateFunDec declaration
    ReturnStatement _ expr          -> do
        val <- evaluateExpression expr
        throwError $ SigReturn val
    -- _ -> undefined

evaluateWhile :: MonadIO m => Expression -> Statement -> Evaluating m ()
evaluateWhile condition body = do
    val <- evaluateExpression condition
    if truthy val then evaluateStatement body >> evaluateWhile condition body
    else pure ()

evaluateFunDec :: MonadIO m => FunctionDeclaration -> Evaluating m ()
evaluateFunDec declaration = do
    (locals,_) <- get
    let name  = funName declaration
        arity = length $ funParams declaration
    modify' $ define name (VCallable arity (UserDefined declaration locals))

-- Helpers
-- Nil is false, False is false, everything else is true.
truthy :: Value -> Bool
truthy = \case
    VBoolean b -> b
    VNil       -> False
    _          -> True

addScope :: Variables -> Variables
addScope (rest,g) = (Map.empty : rest,g)

dropScope :: Variables -> Variables
dropScope (_:rest,g) = (rest,g)
dropScope ([],g)     = ([],g) -- shouldn't ever happen...

error :: MonadError EvalSignal m => Token -> String -> m a
error op message = throwError $ SigError $ EvalError op message