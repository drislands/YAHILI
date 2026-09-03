{-# LANGUAGE LambdaCase #-}

module Parse where

import Control.Monad.Writer
import qualified Data.List.NonEmpty as NE

import Language
import Scan

data ParseError =
    ParseError deriving (Show,Eq)

type Parsing a = Writer [ParseError] a

type Tokens = [Token]

parenthesize :: String -> [Expression] -> String
parenthesize name es =
    "(" <> unwords (name : map uglyPrint es) <> ")"

uglyPrint :: Expression -> String
uglyPrint = \case
    Binary   left op right -> parenthesize (getLexeme op) [left,right]
    Grouping e             -> parenthesize "group" [e]
    Unary    op e          -> parenthesize (getLexeme op) [e]
    LNil                   -> "nil"
    LString  s             -> s
    LBoolean b             -> show b 
    LNumber  n             -> show n

parse :: Tokens -> Parsing Expression
parse tokens = undefined

parseExpression :: Tokens -> Parsing (Expression,Tokens)
parseExpression = parseEquality

parseEquality :: Tokens -> Parsing (Expression,Tokens)
parseEquality tokens = do
    (left,tokens') <- parseComparison tokens
    loop left tokens'
  where
    loop :: Expression -> Tokens -> Parsing (Expression,Tokens)
    loop expr ts = case empty ts of
        Just (t NE.:| rest) | getTokenType t `elem` [Lx_BangEqual,Lx_EqualEqual] -> do
            (right,ts') <- parseComparison rest
            loop (Binary expr t right) ts'
        _ -> pure (expr,ts)


parseComparison = undefined

empty :: Tokens -> Maybe (NE.NonEmpty Token)
empty ts = case NE.nonEmpty ts of
    Nothing -> Nothing
    Just ts'@(t NE.:| _) | getTokenType t == Lx_EOF -> Nothing
                         | otherwise      -> Just ts'