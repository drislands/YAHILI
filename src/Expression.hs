{-# LANGUAGE LambdaCase #-}

module Expression where

import Language

import Control.Monad.State (StateT (runStateT))
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Data.Map (Map)
import qualified Data.Map as Map


data EvalError = 
    EvalError Token String
    deriving (Show)
type Variables = Map.Map String Value

type Evaluating a = StateT Variables (Except EvalError) a

evaluate :: Variables -> Expression -> Either EvalError (Value,Variables)
evaluate vars ex = 
    let result = evaluateInner ex
    in  runExcept (runStateT result vars)

evaluateInner :: Expression -> Evaluating Value
evaluateInner = \case
    -- Literals
    LString  s -> pure $ VString  s
    LBoolean b -> pure $ VBoolean b
    LNumber  d -> pure $ VNumber  d
    LNil       -> pure $ VNil
    -- Expressions!
    Binary left op right -> evaluateBinary left op right
    Grouping e -> evaluateInner e
    Unary op right -> evaluateUnary op right
    Identifier t -> undefined

evaluateUnary :: Token -> Expression -> Evaluating Value
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

evaluateBinary :: Expression -> Token -> Expression -> Evaluating Value
evaluateBinary left op right = case lookupOp (getTokenType op) of
    Just f -> f op left right
    Nothing -> throwError $ EvalError op "Invalid binary operator."
  where
    lookupOp :: TokenType -> Maybe (Token -> Expression -> Expression -> Evaluating Value)
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
evaluateBinaryValues :: ((Value,Value) -> Evaluating Value) -> Expression -> Expression -> Evaluating Value
evaluateBinaryValues f left right = do
    l <- evaluateInner left
    r <- evaluateInner right
    f (l,r)

-- Math (and string concatenation)
evaluatePlus :: Token -> Expression -> Expression -> Evaluating Value
evaluatePlus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1+v2)
    (VString s1,VString s2) -> pure $ VString (s1<>s2)
    _ -> throwError $ EvalError op "Operands must be two numbers or two strings."

evaluateMinus :: Token -> Expression -> Expression -> Evaluating Value
evaluateMinus op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1-v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateStar :: Token -> Expression -> Expression -> Evaluating Value
evaluateStar op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1*v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateSlash :: Token -> Expression -> Expression -> Evaluating Value
evaluateSlash op = evaluateBinaryValues $ \case
    (VNumber _,VNumber 0)  -> throwError $ EvalError op "Cannot divide by zero."
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1/v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

-- Boolean logic
evaluateEquality :: Token -> Expression -> Expression -> Evaluating Value
evaluateEquality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l == r)

evaluateInequality :: Token -> Expression -> Expression -> Evaluating Value
evaluateInequality _ = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l /= r)

-- Nil is false, False is false, everything else is true.
truthy :: Value -> Bool
truthy = \case
    VBoolean b -> b
    VNil       -> False
    _          -> True

-- Number comparison
evaluateLess :: Token -> Expression -> Expression -> Evaluating Value
evaluateLess op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 < v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateLessEqual :: Token -> Expression -> Expression -> Evaluating Value
evaluateLessEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 <= v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateGreater :: Token -> Expression -> Expression -> Evaluating Value
evaluateGreater op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 > v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."

evaluateGreaterEqual :: Token -> Expression -> Expression -> Evaluating Value
evaluateGreaterEqual op = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 >= v2)
    _ -> throwError $ EvalError op "Operands must be two numbers."
