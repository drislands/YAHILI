{-# LANGUAGE LambdaCase #-}

module Scan where

import Control.Monad.Writer

data Token = Token 
    { getTokenType :: TokenType
    , getLexeme    :: String
    -- , getLiteral   :: (Show a) => a
    , getLineNum   :: Int
    }
instance Show Token where
    show t =
        let tt = getTokenType t
            tl = getLineNum t
        in  show tt <> " " <> getLexeme t -- <> " " <> show tl


data TokenType =
    -- Single character
    Lx_LeftParen  | Lx_RightParen   | Lx_LeftBrace | Lx_RightBrace |
    Lx_Comma      | Lx_Dot          | Lx_Minus     | Lx_Plus       |
    Lx_Semicolon  | Lx_Slash        | Lx_Star      |
    -- Single and double
    Lx_Bang       | Lx_BangEqual    | Lx_Equal     | Lx_EqualEqual |
    Lx_Greater    | Lx_GreaterEqual | Lx_Less      | Lx_LessEqual  |
    -- Literals
    Lx_Identifier | Lx_String       | Lx_Number    |
    -- Keywords
    Lx_And        | Lx_Class        | Lx_Else      | Lx_False      |
    Lx_Fun        | Lx_For          | Lx_If        | Lx_Nil        |
    Lx_Or         | Lx_Print        | Lx_Return    | Lx_Super      |
    Lx_This       | Lx_True         | Lx_Var       | Lx_While      |
    -- And EOF by itself
    Lx_EOF
    deriving (Show,Eq)

data LexError =
    UnexpectedChar Int Char
    deriving (Show,Eq)

type Lexing a = Writer [LexError] a


tokensFromSource :: String -> Lexing [Token]
tokensFromSource src = do
    (ts,n,_) <- tokensFromSource' ([],1,src)
    let finalEOF = Token { getTokenType=Lx_EOF,getLexeme="",getLineNum=n+1 }
    case length ts of
        0 -> pure [finalEOF]
        _ -> pure $ reverse $ finalEOF : ts

tokensFromSource' :: ([Token],Int,String) -> Lexing ([Token],Int,String)
tokensFromSource' (ts,n,"") = pure (ts,n,"")
tokensFromSource' (ts,n,s) = do
    scanned <- scanToken s
    case scanned of
        Left (n',ss) -> tokensFromSource' (ts,n',ss)
        Right (token,n',ss) -> tokensFromSource' (token:ts,n',ss)
  where
    scanToken :: String -> Lexing  (Either (Int,String) (Token,Int,String))
    scanToken "" = undefined
    scanToken (x:xs) = do
        mtt <- getType
        case mtt of
            Nothing -> pure $ Left (n,xs)
            Just tt -> do
                let token = Token { getTokenType = tt, getLexeme = [x], getLineNum = n}
                pure $ Right (token,n,xs)
      where
        getType :: Lexing (Maybe TokenType)
        getType = case charToToken x of
            Just tok -> pure (Just tok)
            Nothing  -> do
                tell [UnexpectedChar n x]
                pure Nothing
        charToToken :: Char -> Maybe TokenType
        charToToken = \case
            '(' -> Just Lx_LeftParen
            ')' -> Just Lx_RightParen
            '{' -> Just Lx_LeftBrace
            '}' -> Just Lx_RightBrace
            ',' -> Just Lx_Comma
            '.' -> Just Lx_Dot
            '-' -> Just Lx_Minus
            '+' -> Just Lx_Plus
            ';' -> Just Lx_Semicolon
            _   -> Nothing