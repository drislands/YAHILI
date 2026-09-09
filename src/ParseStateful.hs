{-# LANGUAGE LambdaCase #-}

module ParseStateful where

import Prelude hiding (head,tail)

import Control.Monad.Writer
import Control.Monad.State
-- import GHC.Pre

import Language
import Control.Monad (when, unless)
import Data.Maybe (isNothing)

data ParseError =
    ParseError Token String 
    deriving (Show)

type Parsing a = StateT Tokens (Writer [ParseError]) a


parenthesize :: String -> [Expression] -> String
parenthesize name es =
    "(" <> unwords (name : map uglyPrint es) <> ")"

uglyPrint :: Expression -> String
uglyPrint = \case
    Ternary  left op1 mid op2 right -> parenthesize (getLexeme op1 <> getLexeme op2) [left,mid,right]
    Binary   left op right          -> parenthesize (getLexeme op) [left,right]
    Grouping e                      -> parenthesize "group" [e]
    Unary    op e                   -> parenthesize (getLexeme op) [e]
    LNil                            -> "nil"
    LString  s                      -> s
    LBoolean b                      -> show b 
    LNumber  n                      -> show n

parse :: Tokens -> Writer [ParseError] Expression
parse = evalStateT parseExpression

parseExpression :: Parsing Expression
parseExpression = parseComma

parseComma :: Parsing Expression
parseComma = parseBinary parseTernary [Lx_Comma]

parseTernary :: Parsing Expression
parseTernary = do
    left <- parseEquality
    mq   <- match [Lx_Question]
    case mq of
        Just q -> do
            mid   <- parseTernary
            c     <- consume Lx_Colon "Expect ':' after expression."
            right <- parseTernary
            pure $ Ternary left q mid c right
        Nothing -> pure left

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
        Just e  -> do
            advance
            pure e
        Nothing -> do
            match [Lx_LeftParen] >>= \case
                Just _ -> do
                    e <- parseExpression
                    consume_ Lx_RightParen "Expect ')' after expression."
                    pure e
                Nothing -> do
                    e <- peek
                    lift $ tell [ParseError e "Expect expression."]
                    pure LNil

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
consume :: TokenType -> String -> Parsing Token
consume tt message = do
    matched <- match [tt]
    case matched of
        Just good -> pure good
        Nothing -> do
            bad <- peek
            lift $ tell [ParseError bad message]
            pure bad

consume_ :: TokenType -> String -> Parsing ()
consume_ tt message = do
    _ <- consume tt message
    pure ()

synchronize :: Parsing ()
synchronize = do
    advance
    t <- peek
    unless (statementTerm t) synchronize
  where
    statementTerm :: Token -> Bool
    statementTerm t = getTokenType t `elem` 
        [ Lx_Class
        , Lx_Fun
        , Lx_Var
        , Lx_If
        , Lx_While
        , Lx_Print
        , Lx_Return
        ]

