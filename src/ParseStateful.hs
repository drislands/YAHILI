{-# LANGUAGE LambdaCase #-}

module ParseStateful where

import Prelude hiding (head,tail)

import Control.Monad.Writer
import Control.Monad.State
-- import GHC.Pre

import Language

data ParseError =
    ParseError Token String 
    deriving (Show)

type Parsing a = StateT Tokens (Writer [ParseError]) a


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

parse :: Tokens -> Writer [ParseError] Expression
parse = evalStateT parseExpression

parseExpression :: Parsing Expression
parseExpression = parseEquality

parseEquality :: Parsing Expression
parseEquality = parseBinary parseComparison [Lx_BangEqual,Lx_EqualEqual]

parseComparison :: Parsing Expression
parseComparison = parseBinary parseTerm [Lx_Greater,Lx_GreaterEqual,Lx_Less,Lx_LessEqual]

parseTerm :: Parsing Expression
parseTerm = parseBinary parseFactor [Lx_Minus,Lx_Plus]

parseFactor :: Parsing Expression
parseFactor = parseBinary parseUnary [Lx_Slash,Lx_Star]

parseBinary :: Parsing Expression -> [TokenType] -> Parsing Expression
parseBinary nextFunc types = do
    left <- nextFunc
    loop left
  where
    loop :: Expression -> Parsing Expression
    loop expr = do
        match types >>= \case
            Just t -> do
                right <- nextFunc
                loop (Binary expr t right)
            Nothing -> pure expr

parseUnary :: Parsing Expression
parseUnary = do
    match [Lx_Bang,Lx_Minus] >>= \case
        Just t -> do
            right <- parseUnary
            pure $ Unary t right
        Nothing -> parsePrimary

parsePrimary :: Parsing Expression
parsePrimary = do
    t <- peek
    case matchLit t of
        Just e  -> pure e
        Nothing -> do
            match [Lx_LeftParen] >>= \case
                Just _ -> do
                    e <- parseExpression
                    consume Lx_RightParen "Expect ')' after expression."
                    pure e
                Nothing -> undefined

  where
    matchLit :: Token -> Maybe Expression
    matchLit t =
        let tt = getTokenType t
            tl = getLexeme    t
        in  case tt of
            Lx_False     -> Just (LBoolean False)
            Lx_True      -> Just (LBoolean True)
            Lx_Nil       -> Just (LNil)
            Lx_String    -> Just (LString tl)
            Lx_Number    -> Just (LNumber (read tl))
            _            -> Nothing


-- Helper stateful functions.
match :: [TokenType] -> Parsing (Maybe Token)
match types = do
    t <- peek
    if getTokenType t `elem` types then do
        advance
        pure (Just t)
    else pure Nothing

previous :: Parsing Token
previous = undefined

advance :: Parsing ()
advance = do
    rest <- get
    put $ tail rest

atEnd :: Parsing Bool
atEnd = do
    ts <- get
    case ts of
        TksLast _ -> pure True
        _         -> pure False

peek :: Parsing Token
peek = do
    tokens <- get
    pure $ head tokens

-- Error handling!
consume = undefined