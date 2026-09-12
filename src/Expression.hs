{-# LANGUAGE LambdaCase #-}

module Expression where

import Language

import Control.Monad.State (StateT (runStateT), MonadState (get, put), MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Prelude hiding (lookup)


type Evaluating m a = StateT Variables (ExceptT EvalError m) a

evaluate :: MonadIO m => Variables -> Expression -> m (Either EvalError (Value,Variables))
evaluate vars ex = 
    let result = evaluateInner ex
    in  runExceptT (runStateT result vars)

evaluateInner :: MonadIO m => Expression -> Evaluating m Value
evaluateInner = \case
    -- Literals
    LString  s -> pure $ VString  s
    LBoolean b -> pure $ VBoolean b
    LNumber  d -> pure $ VNumber  d
    LNil       -> pure $ VNil
    -- Expressions!
    Binary left op right -> evaluateBinary left op right
    Call callee t args -> do
        callee' <- evaluateInner callee
        case callee' of
            VCallable arity callable -> do
                if arity /= length args then throwError $ EvalError t ("Expected " <> 
                    show arity <> " arguments but got " <> 
                    show (length args) <> ".")
                else do
                    args' <- mapM evaluateInner args
                    case callable of
                        UserDefined body -> undefined
                        NativeFunction iofunc -> do
                            result <- lift $ liftIO (iofunc args')
                            case result of
                                Left er -> throwError er
                                Right val -> pure val
            _ -> throwError $ EvalError t 
                ("Can only call functions and classes.")
    Grouping e -> evaluateInner e
    Unary op right -> evaluateUnary op right
    Assignment t val -> do
        let name = getLexeme t
        vars <- get
        val' <- evaluateInner val
        case assign name val' vars of
            Just vars' -> do
                put vars'
                pure val'
            Nothing -> throwError $ EvalError t
                ("Undefined variable '" <> name <> "'.")
    Identifier t -> do
        let name = getLexeme t
        vars <- get
        case lookup name vars of
            Just x -> pure x
            Nothing -> throwError $ EvalError t 
                ("Undefined variable '" <> name <> "'.")
    -- More complex stuff, like control flow!
    Logical left op right -> do
        left' <- evaluateInner left
        if (getTokenType op == Lx_Or) == truthy left' then
            pure left'
        else evaluateInner right
            

evaluateUnary :: MonadIO m => Token -> Expression -> Evaluating m Value
evaluateUnary op right = do
    case getTokenType op of
        Lx_Minus -> do
            r <- evaluateInner right
            case r of
                VNumber d -> pure $ VNumber (0 - d)
                _ -> throwError $ EvalError op "Operand must be a number."
        Lx_Bang  -> do
            r <- evaluateInner right
            pure $ VBoolean ((not . truthy) r)
        _ -> throwError $ EvalError op "Invalid unary operator."

evaluateBinary :: MonadIO m => Expression -> Token -> Expression -> Evaluating m Value
evaluateBinary left op right = case lookupOp (getTokenType op) of
    Just f -> f op left right
    Nothing -> throwError $ EvalError op "Invalid binary operator."
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

-- Binary functions
evaluateBinaryValues :: MonadIO m => ((Value,Value) -> Evaluating m Value) -> Expression -> Expression -> Evaluating m Value
evaluateBinaryValues f left right = do
    l <- evaluateInner left
    r <- evaluateInner right
    f (l,r)

-- Math (and string concatenation)
evaluatePlus :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluatePlus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1+v2)
    (VString s1,VString s2) -> pure $ VString (s1<>s2)
    _ -> throwError $ EvalError op "Operands must be two numbers or two strings."

evaluateMinus :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateMinus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1-v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateStar :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateStar op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1*v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateSlash :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateSlash op = evaluateBinaryValues $ \case
    (VNumber _,VNumber 0)  -> throwError $ EvalError op "Cannot divide by zero."
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1/v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

-- Boolean logic
evaluateEquality :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateEquality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l == r)

evaluateInequality :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateInequality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l /= r)

-- Nil is false, False is false, everything else is true.
truthy :: Value -> Bool
truthy = \case
    VBoolean b -> b
    VNil       -> False
    _          -> True

-- Number comparison
evaluateLess :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateLess op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 < v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateLessEqual :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateLessEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 <= v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateGreater :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateGreater op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 > v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateGreaterEqual :: MonadIO m => Token -> Expression -> Expression -> Evaluating m Value
evaluateGreaterEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 >= v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."
