{-# LANGUAGE LambdaCase #-}

module Evaluate where

import Language

import Control.Monad.State (StateT)
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map


data EvalError = EvalError
type Variables = Map.Map String Value

type Evaluating a = StateT Variables (Writer [EvalError]) a

evaluate :: Expression -> Evaluating Value
evaluate = \case
    -- Literals
    LString  s -> pure $ VString  s
    LBoolean b -> pure $ VBoolean b
    LNumber  d -> pure $ VNumber  d
    LNil       -> pure $ VNil
    -- Expressions!
    Binary left op right -> evaluateBinary left op right
    Grouping e -> evaluate e
    Unary op right -> evaluateUnary op right

evaluateUnary :: Token -> Expression -> Evaluating Value
evaluateUnary op right = do
    case getTokenType op of
        Lx_Minus -> do
            r <- evaluate right
            case r of
                VNumber d -> pure $ VNumber (0 - d)
                _ -> undefined
        Lx_Bang  -> do
            r <- evaluate right
            pure $ VBoolean ((not . truthy) r)
        _ -> undefined

evaluateBinary :: Expression -> Token -> Expression -> Evaluating Value
evaluateBinary left op right = case lookupOp (getTokenType op) of
    Just f -> f left right
    Nothing -> do
        lift $ tell [EvalError]
        pure VNil
  where
    lookupOp :: TokenType -> Maybe (Expression -> Expression -> Evaluating Value)
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
    l <- evaluate left
    r <- evaluate right
    f (l,r)

-- Math (and string concatenation)
evaluatePlus :: Expression -> Expression -> Evaluating Value
evaluatePlus = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1+v2)
    (VString s1,VString s2) -> pure $ VString (s1<>s2)
    _ -> undefined

evaluateMinus :: Expression -> Expression -> Evaluating Value
evaluateMinus = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1-v2)
    _ -> undefined

evaluateStar :: Expression -> Expression -> Evaluating Value
evaluateStar = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1*v2)
    _ -> undefined

evaluateSlash :: Expression -> Expression -> Evaluating Value
evaluateSlash = evaluateBinaryValues $ \case
    (VNumber _,VNumber 0)  -> undefined
    (VNumber v1,VNumber v2) -> pure $ VNumber (v1/v2)
    _ -> undefined

-- Boolean logic
evaluateEquality :: Expression -> Expression -> Evaluating Value
evaluateEquality = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l == r)

evaluateInequality :: Expression -> Expression -> Evaluating Value
evaluateInequality = evaluateBinaryValues $ \(l,r) -> pure $ VBoolean(l /= r)

-- Nil is false, False is false, everything else is true.
truthy :: Value -> Bool
truthy = \case
    VBoolean b -> b
    VNil       -> False
    _          -> True

-- Number comparison
evaluateLess :: Expression -> Expression -> Evaluating Value
evaluateLess = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 < v2)
    _ -> undefined

evaluateLessEqual :: Expression -> Expression -> Evaluating Value
evaluateLessEqual = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 <= v2)
    _ -> undefined

evaluateGreater :: Expression -> Expression -> Evaluating Value
evaluateGreater = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 > v2)
    _ -> undefined

evaluateGreaterEqual :: Expression -> Expression -> Evaluating Value
evaluateGreaterEqual = evaluateBinaryValues $ \case
    (VNumber v1,VNumber v2) -> pure $ VBoolean (v1 >= v2)
    _ -> undefined
