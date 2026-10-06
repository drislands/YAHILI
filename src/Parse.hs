{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse where

import Prelude hiding (head,tail)

import Effectful
import Effectful.State.Static.Local
import Effectful.Writer.Static.Local

import Language
import Control.Monad (unless)

data ParseError =
    ParseError Token String 
    deriving (Show)

type Parsing es a = (State Tokens :> es, Writer [ParseError] :> es ) => Eff es a


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
    Assignment t e         -> parenthesize (getLexeme t <> "=") [e]
    Identifier t           -> getLexeme t
    Logical l op r         -> parenthesize (getLexeme op) [l,r]
    Call c _ args          -> parenthesize "call" (c:args)


parseExpression :: Parsing es Expression
parseExpression = parseAssignment

parseAssignment :: Parsing es Expression
parseAssignment = do
    expr <- parseOr
    m <- match [Lx_Equal]
    case m of
        Nothing -> pure expr
        Just eq -> do
            case expr of
                Identifier v -> do
                    value <- parseAssignment
                    pure $ Assignment v value
                _            -> do
                    tell [ParseError eq "Invalid assignment target."]
                    pure expr

parseOr :: Parsing es Expression
parseOr = do
    left <- parseAnd
    m <- match [Lx_Or]
    case m of
        Nothing -> pure left
        Just op  -> do
            right <- parseOr
            pure $ Logical left op right

parseAnd :: Parsing es Expression
parseAnd = do
    left <- parseEquality
    m <- match [Lx_And]
    case m of
        Nothing -> pure left
        Just op  -> do
            right <- parseAnd
            pure $ Logical left op right

parseEquality :: Parsing es Expression
parseEquality = parseBinary parseComparison [Lx_BangEqual,Lx_EqualEqual]

parseComparison :: Parsing es Expression
parseComparison = parseBinary parseTerm [Lx_Greater,Lx_GreaterEqual,Lx_Less,Lx_LessEqual]

parseTerm :: Parsing es Expression
parseTerm = parseBinary parseFactor [Lx_Minus,Lx_Plus]

parseFactor :: Parsing es Expression
parseFactor = parseBinary parseUnary [Lx_Slash,Lx_Star]

parseBinary :: Parsing es Expression -> [TokenType] -> Parsing es Expression
parseBinary nextFunc types = do
    left <- nextFunc
    loop left
  where
    loop expr = do
        match types >>= \case
            Just t -> do
                right <- nextFunc
                loop (Binary expr t right)
            Nothing -> pure expr

parseUnary :: Parsing es Expression
parseUnary = do
    match [Lx_Bang,Lx_Minus] >>= \case
        Just t -> do
            right <- parseUnary
            pure $ Unary t right
        Nothing -> parseCall

parseCall :: Parsing es Expression
parseCall = do
    expr <- parsePrimary
    finishCall expr
  where
    finishCall :: Expression -> Parsing es Expression
    finishCall callee = do
        m <- match [Lx_LeftParen]
        case m of
            Nothing -> pure callee
            Just _  -> do
                rp <- peek
                margs <- if getTokenType rp == Lx_RightParen
                    then pure $ Right []
                    else parseArgs 1
                case margs of
                    Left t -> do
                        tell [ParseError t "Can't have more than 255 arguments."]
                        synchronize
                        pure LNil
                    Right args -> do
                        paren <- consume Lx_RightParen "Expect ')' after arguments."
                        finishCall (Call callee paren args)

    parseArgs :: Int -> Parsing es (Either Token [Expression])
    parseArgs n = do
        if n > 255 then do
            t <- peek
            pure $ Left t
        else do
            arg <- parseExpression
            m <- match [Lx_Comma]
            case m of
                Nothing -> pure $ Right [arg]
                Just _  -> (fmap . fmap) (arg :) (parseArgs (n+1))

parsePrimary :: Parsing es Expression
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
                    pure $ Grouping e
                Nothing -> do
                    e <- peek
                    tell [ParseError e "Expect expression."]
                    pure LNil

  where
    matchLit :: Token -> Maybe Expression
    matchLit t =
        let tt = getTokenType t
            tl = getLexeme    t
        in  case tt of
            Lx_False      -> Just (LBoolean False)
            Lx_True       -> Just (LBoolean True)
            Lx_Nil        -> Just (LNil)
            Lx_String     -> Just (LString tl)
            Lx_Number     -> Just (LNumber (read tl))
            Lx_Identifier -> Just (Identifier t)
            _             -> Nothing


-- Helper stateful functions.
match :: [TokenType] -> Parsing es (Maybe Token)
match types = do
    t <- peek
    if getTokenType t `elem` types then do
        advance
        pure (Just t)
    else pure Nothing

advance :: Parsing es ()
advance = do
    rest <- get
    put $ tail rest

atEnd :: Parsing es Bool
atEnd = do
    ts <- get
    case ts of
        TksLast _ -> pure True
        _         -> pure False

peek :: Parsing es Token
peek = do
    tokens <- get
    pure $ head tokens

-- Error handling!
consume :: TokenType -> String -> Parsing es Token
consume tt message = do
    matched <- match [tt]
    case matched of
        Just good -> pure good
        Nothing -> do
            bad <- peek
            tell [ParseError bad message]
            synchronize
            pure bad

consume_ :: TokenType -> String -> Parsing es ()
consume_ tt message = do
    _ <- consume tt message
    pure ()

synchronize :: Parsing es ()
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
        , Lx_Semicolon
        , Lx_EOF
        ]

