{-# LANGUAGE PatternSynonyms #-}

module Language
    ( TokenType(..)
    , Token(..)
    , Expression(..)
    , Tokens
    , mkTokens
    , pattern TksLast
    , pattern (:|)
    ) where



data Token = Token 
    { getTokenType :: TokenType
    , getLexeme    :: String
    -- , getLiteral   :: (Show a) => a
    , getLineNum   :: Int
    }
instance Show Token where
    show t =
        let tt = getTokenType t
            -- tl = getLineNum t
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

data Expression =
    -- Recursive expressions
    Binary   Expression Token Expression | -- 4 + 3, i * j, etc
    Grouping Expression                  | -- ( 4 ), ( 4 + 3), etc
    Unary    Token Expression            | -- - 4, etc
    -- Literals
    LString  String                      | -- "etc", etc
    LBoolean Bool                        | -- true, false
    LNumber  Double                      | -- 4, 4.3, etc
    LNil                                   -- nil
    deriving (Show)
    -- TODO: Variables?

-- -----
-- Specialized token list handling to guarantee that every list
-- ends with an EOF.
newtype EOFToken = UnsafeEOFToken { getEOF :: Token } deriving (Show)
mkEOF :: Token -> Maybe EOFToken
mkEOF t 
    | getTokenType t == Lx_EOF = Just (UnsafeEOFToken t)
    | otherwise                = Nothing

newtype BodyToken = UnsafeBodyToken { getBodyToken :: Token } deriving (Show)
mkBody :: Token -> Maybe BodyToken
mkBody t
    | getTokenType t /= Lx_EOF = Just (UnsafeBodyToken t)
    | otherwise                = Nothing

infixr 5 :|*

data Tokens 
    = TksLastInternal EOFToken
    | BodyToken :|* Tokens
    deriving (Show)

pattern TksLast :: Token -> Tokens
pattern TksLast t <- TksLastInternal (UnsafeEOFToken t)

pattern (:|) :: Token -> Tokens -> Tokens
pattern t :| rest <- UnsafeBodyToken t :|* rest

-- Informs GHC that matching on (:|) and TksLast covers all cases of Tokens
{-# COMPLETE (:|), TksLast #-}

mkTokens :: [Token] -> Maybe Tokens
mkTokens []     = Nothing
mkTokens [t]    = TksLastInternal <$> mkEOF t
mkTokens (t:ts) = (:|*) <$> mkBody t <*> mkTokens ts
-- -----