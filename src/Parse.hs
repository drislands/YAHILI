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
parseEquality = parseBinary parseComparison [Lx_BangEqual,Lx_EqualEqual]

parseComparison :: Tokens -> Parsing (Expression,Tokens)
parseComparison = parseBinary parseTerm [Lx_Greater,Lx_GreaterEqual,Lx_Less,Lx_LessEqual]

parseTerm :: Tokens -> Parsing (Expression, Tokens)
parseTerm = parseBinary parseFactor [Lx_Minus,Lx_Plus]

parseFactor :: Tokens -> Parsing (Expression, Tokens)
parseFactor = parseBinary parseUnary [Lx_Slash,Lx_Star]

parseUnary = undefined

-- For Binary expressions, the form is ultimately the same: call the next function up in the
-- precedence ladder, then possibly loop on the results depending on if this function's
-- operator is found.
parseBinary :: (Tokens -> Parsing (Expression,Tokens)) -> [TokenType] -> Tokens -> Parsing (Expression,Tokens)
parseBinary nextFunc types tokens = do
    (left,tokens') <- nextFunc tokens
    loop left tokens'
  where
    loop :: Expression -> Tokens -> Parsing (Expression,Tokens)
    loop expr ts = case empty ts of
        Just (t NE.:| rest) | getTokenType t `elem` types -> do
            (right,ts') <- nextFunc rest
            loop (Binary expr t right) ts'
        _ -> pure (expr,ts)

-- For our purposes, hitting an EOF is the same as hitting the end of the list.
empty :: Tokens -> Maybe (NE.NonEmpty Token)
empty ts = case NE.nonEmpty ts of
    Nothing -> Nothing
    Just ts'@(t NE.:| _) | getTokenType t == Lx_EOF -> Nothing
                         | otherwise      -> Just ts'