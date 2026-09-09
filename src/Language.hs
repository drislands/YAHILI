{-# LANGUAGE PatternSynonyms #-}

module Language
    ( TokenType(..)
    , Token(..)
    , Expression(..)
    , Tokens
    , Variables
    , define
    , lookup
    , Value(..)
    , Statement(..)
    , Program
    , head
    , tail
    , mkTokens
    , pattern TksLast
    , pattern (:|)
    ) where

import Prelude hiding (head,tail,lookup)
import Data.Int
import qualified Data.Map as Map

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
    LNil                                 | -- nil
    -- Variables!
    Assignment Token Expression          | -- p = 9;
    Identifier Token
    deriving (Show)

type Variables = Map.Map String Value

define :: String -> Value -> Variables -> Variables
define = Map.insert

lookup :: String -> Variables -> Maybe Value
lookup = Map.lookup

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

head :: Tokens -> Token
head (TksLastInternal eof) = getEOF eof
head (b :|* _) = getBodyToken b

tail :: Tokens -> Tokens
tail e@(TksLastInternal _) = e
tail (_ :|* rest) = rest

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

data Value =
    VString  String |
    VBoolean Bool   |
    VNumber  Double |
    VNil
    deriving (Eq,Ord)

instance Show Value where
    show (VString s)  = s
    show (VBoolean b) = show b
    show (VNumber d)  =
        if isInteger d then show (truncate d :: Int64)
        else show d
    show VNil         = "nil"

isInteger :: Double -> Bool
isInteger d
    | isNaN d || isInfinite d   = False
    | abs d >= 9007199254740992 = True
    | otherwise                 = d == fromIntegral (truncate d :: Int64)

-- Statements!
data Statement =
    VarDeclaration String Expression |
    ExpressionStatement   Expression |
    PrintStatement        Expression
    deriving (Show)

type Program = [Statement]
    