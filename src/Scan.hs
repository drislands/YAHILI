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
    scanToken (x:"") = do
        (mtt,_) <- getType x Nothing
        case mtt of
            Nothing -> pure $ Left (n,"")
            Just tt -> do
                let token = Token { getTokenType = tt, getLexeme = [x], getLineNum = n}
                pure $ Right (token,n,"")
    scanToken (x:y:ys) = do
        (mtt,wasTwo) <- getType x (Just y)
        let nextString = if wasTwo then ys else y:ys
            lexeme     = if wasTwo then [x,y] else [x]
        case mtt of
            Nothing -> pure $ Left (n,nextString)
            Just tt -> do
                let token = Token { getTokenType = tt, getLexeme = lexeme, getLineNum = n}
                pure $ Right (token,n,nextString)
    getType :: Char -> Maybe Char -> Lexing (Maybe TokenType,Bool)
    getType x Nothing = case charToToken x of
        Just tok -> pure (Just tok,False)
        Nothing  -> do
            tell [UnexpectedChar n x]
            pure (Nothing,False)
    getType x (Just y) = case charsToToken x y of
        (Just tok,b) -> pure (Just tok,b)
        (Nothing,b) -> do
            tell [UnexpectedChar n x]
            pure (Nothing,b)

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
        '*' -> Just Lx_Star
        _   -> Nothing
    charsToToken :: Char -> Char -> (Maybe TokenType,Bool)
    charsToToken x y = 
        let lx_isEquals = y == '='
        in  case x of
            '!' -> if lx_isEquals then (Just Lx_BangEqual,True) else (Just Lx_Bang,False)
            '=' -> if lx_isEquals then (Just Lx_EqualEqual,True) else (Just Lx_Equal,False)
            '<' -> if lx_isEquals then (Just Lx_LessEqual,True) else (Just Lx_Less,False)
            '>' -> if lx_isEquals then (Just Lx_GreaterEqual,True) else (Just Lx_Greater,False)
            _ -> (charToToken x,False)