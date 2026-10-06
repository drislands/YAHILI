{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Interpreter where

import Language

import Prelude hiding (error, lookup)
import qualified Data.Map as Map
import Effectful (Eff, (:>), IOE, liftIO)
import Effectful.State.Static.Local
import Effectful.Error.Static


data EvalSignal = 
    SigError EvalError |
    SigReturn Value
    deriving (Show)
type Evaluating es a = (State Variables :> es, Error EvalSignal :> es, IOE :> es) => Eff es a

-- Expression evaluation first.
evaluateExpression :: Expression -> Evaluating es Value
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

evaluateBinary :: Expression -> Token -> Expression -> Evaluating es Value
evaluateBinary left op right = case lookupOp (getTokenType op) of
    Just f -> f op left right
    Nothing -> error op "Invalid binary operator."
  where
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

evaluateUnary :: Token -> Expression -> Evaluating es Value
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

evaluateAssignment :: Token -> Expression -> Evaluating es Value
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

evaluateIdentifier :: Token -> Evaluating es Value
evaluateIdentifier t = do
    let name = getLexeme t
    vars <- get
    case lookup name vars of
        Just x -> pure x
        Nothing -> error t 
            ("Undefined variable '" <> name <> "'.")

evaluateLogical :: Expression -> Token -> Expression -> Evaluating es Value
evaluateLogical left op right = do
    left' <- evaluateExpression left
    if (getTokenType op == Lx_Or) == truthy left' then
        pure left'
    else evaluateExpression right

evaluateCall :: Expression -> Token -> [Expression] -> Evaluating es Value
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
                        (callerLocals,g) <- get @Variables
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
                        result <- liftIO (iofunc args')
                        case result of
                            Left er -> throwError $ SigError er
                            Right val -> pure val
        _ -> error t 
            ("Can only call functions and classes.")
  where
    handleSignal :: CallStack -> EvalSignal -> Evaluating es Value
    handleSignal _ = \case
        SigError er -> throwError $ SigError er
        SigReturn val -> pure val
    runClosure :: Statement -> Evaluating es (Value,Variables)
    runClosure body = do
        val <- (evaluateStatement body >> pure VNil) `catchError` handleSignal
        (locals,g) <- get
        case locals of
            _ : rest -> pure (val,(rest,g))
            rest     -> pure (val,(rest,g))


-- Binary functions
evaluateBinaryValues :: ((Value,Value) -> Evaluating es Value) -> Expression -> Expression -> Evaluating es Value
evaluateBinaryValues f left right = do
    l <- evaluateExpression left
    r <- evaluateExpression right
    f (l,r)

-- Math (and string concatenation)
evaluatePlus :: Token -> Expression -> Expression -> Evaluating es Value
evaluatePlus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1+v2)
    (VString s1,VString s2) -> pure $ VString (s1<>s2)
    _ -> error op "Operands must be two numbers or two strings."

evaluateMinus :: Token -> Expression -> Expression -> Evaluating es Value
evaluateMinus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1-v2)
    _ -> error op "Operands must be two numbers."

evaluateStar :: Token -> Expression -> Expression -> Evaluating es Value
evaluateStar op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1*v2)
    _ -> error op "Operands must be two numbers."

evaluateSlash :: Token -> Expression -> Expression -> Evaluating es Value
evaluateSlash op = evaluateBinaryValues $ \case
    (VNumber _,VNumber 0)  -> error op "Cannot divide by zero."
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1/v2)
    _ -> error op "Operands must be two numbers."

-- Boolean logic
evaluateEquality :: Token -> Expression -> Expression -> Evaluating es Value
evaluateEquality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l == r)

evaluateInequality :: Token -> Expression -> Expression -> Evaluating es Value
evaluateInequality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l /= r)

-- Number comparison
evaluateLess :: Token -> Expression -> Expression -> Evaluating es Value
evaluateLess op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 < v2)
    _ -> error op "Operands must be two numbers."

evaluateLessEqual :: Token -> Expression -> Expression -> Evaluating es Value
evaluateLessEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 <= v2)
    _ -> error op "Operands must be two numbers."

evaluateGreater :: Token -> Expression -> Expression -> Evaluating es Value
evaluateGreater op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 > v2)
    _ -> error op "Operands must be two numbers."

evaluateGreaterEqual :: Token -> Expression -> Expression -> Evaluating es Value
evaluateGreaterEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 >= v2)
    _ -> error op "Operands must be two numbers."


-- Statement evaluation next.
evaluateStatement :: Statement -> Evaluating es ()
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
        modify addScope
        mapM_ evaluateStatement statements
        modify dropScope
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

evaluateWhile :: Expression -> Statement -> Evaluating es ()
evaluateWhile condition body = do
    val <- evaluateExpression condition
    if truthy val then evaluateStatement body >> evaluateWhile condition body
    else pure ()

evaluateFunDec :: FunctionDeclaration -> Evaluating es ()
evaluateFunDec declaration = do
    (locals,_) <- get @Variables
    let name  = funName declaration
        arity = length $ funParams declaration
    modify $ define name (VCallable arity (UserDefined declaration locals))

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

error :: Token -> String -> Evaluating es a
error op message = throwError $ SigError $ EvalError op message