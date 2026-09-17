{-# LANGUAGE PatternSynonyms #-}

{- |Contains definitions for various aspects of the language:
    `Token`s and their dedicated group type `Tokens`,
    `Expression`s, `Variables`, and more.
-}
module Language
    ( TokenType(..)
    , Token(..)
    , Expression(..)
    , EvalError(EvalError)
    , Tokens
    , Variables
    , Scope
    , Locals
    , define
    , assign
    , lookup
    , onLocals
    , onGlobals
    , Value(..)
    , LoxCallable(..)
    , Statement(..)
    , Program
    , head
    , tail
    , mkTokens
    , pattern TksLast
    , pattern (:|)
    , FunctionDeclaration(..)
    ) where

import Prelude hiding (head,tail,lookup)
import Data.Int
import qualified Data.Map as Map
import Text.Printf

-- |Represents a Lox token, such as 'for', ';', 'myVar', '"hello"', etc
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

-- |The various kinds of `Token`s allowed. These are operators,
--  punctuation (parentheses, braces, etc), keywords, literals,
--  and the EndOfFile character that indicates the whole body 
--  of code has been read.
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

-- |Represents a Lox expression. A `Binary` expression, for example,
--  consists of two other expressions and an operator joining them,
--  such as "2 + 2" or "a + 5 > b".
data Expression =
    -- Recursive expressions
    Binary   Expression Token Expression | -- 4 + 3, i * j, etc
    Grouping Expression                  | -- ( 4 ), ( 4 + 3), etc
    Unary    Token Expression            | -- - 4, etc
    Logical  Expression Token Expression | -- true or false, etc
    -- Literals
    LString  String                      | -- "etc", etc
    LBoolean Bool                        | -- true, false
    LNumber  Double                      | -- 4, 4.3, etc
    LNil                                 | -- nil
    -- Variables!
    Assignment Token Expression          | -- p = 9;
    Identifier Token                     |
    Call Expression Token [Expression]     -- f ( g, h) etc
    deriving (Show)

-- |Represents an error encountered while evaluating `Tokens`.
--  In essence, syntax errors.
data EvalError = 
    EvalError Token String
    deriving (Show)

-- |A `Map.Map` of variable names and associate `Value`s.
type Scope = Map.Map String Value
type Globals = Scope
type Locals  = [Scope]
-- |The whole set of `Scope`s for the current state of the program.
--  The first value is all non-global `Scope`s from most local up,
--  and the second value is the global `Scope`.
type Variables = (Locals,Globals)

-- |Defines a variable with a `Value`. If the variable
--  already exists in the lowest scope, it is equivalent
--  to calling `assign`.
define :: String -> Value -> Variables -> Variables
define k v (vars:rest,g) = 
    let vars' = Map.insert k v vars
    in  (vars' : rest,g)
define k v ([],g) =
    let g' = Map.insert k v g
    in  ([],g')

-- |Assigns a `Value` to an existing variable in any `Scope`.
--  Returns `Nothing` if the variable does not exist.
assign :: String -> Value -> Variables -> Maybe Variables
assign k v (vars:rest,g) =
    if Map.member k vars
    then Just $ (Map.insert k v vars : rest,g)
    else (onLocals (vars:)) <$> assign k v (rest,g)
assign k v ([],g) =
    if Map.member k g
    then Just $ ([],Map.insert k v g)
    else Nothing

-- |Obtains the `Value` associated with a variable name.
--  If it can't be found at the lowest `Scope`, recurse
--  up until we check the global `Scope`.
lookup :: String -> Variables -> Maybe Value
lookup k (vars:rest,g) =
    case Map.lookup k vars of
        Nothing -> lookup k (rest,g)
        Just r  -> Just r
lookup k ([],g) = Map.lookup k g

onLocals :: (Locals -> Locals) -> Variables -> Variables
onLocals  f (locals,g) = (f locals,g)

onGlobals :: (Globals -> Globals) -> Variables -> Variables
onGlobals f (locals,g) = (locals,f g)

-- -----
-- |A `Token` of `TokenType` `Lx_EOF`. Attempting to construct this
--  with any other type fails with `Nothing`.
newtype EOFToken = UnsafeEOFToken { getEOF :: Token } deriving (Show)
mkEOF :: Token -> Maybe EOFToken
mkEOF t 
    | getTokenType t == Lx_EOF = Just (UnsafeEOFToken t)
    | otherwise                = Nothing

-- |A `Token` of any `TokenType` *except* `Lx_EOF`. Attempting to 
--  construct this with EOF fails with `Nothing`.
newtype BodyToken = UnsafeBodyToken { getBodyToken :: Token } deriving (Show)
mkBody :: Token -> Maybe BodyToken
mkBody t
    | getTokenType t /= Lx_EOF = Just (UnsafeBodyToken t)
    | otherwise                = Nothing

infixr 5 :|*

-- |Specialized token list handling to guarantee that every list
--  ends with an EOF.
data Tokens 
    = TksLastInternal EOFToken
    | BodyToken :|* Tokens
    deriving (Show)

-- |Obtain the Token at the front of the list
--  of Tokens. Since Tokens must always end with
--  an EOF, this will always return a value.
head :: Tokens -> Token
head (TksLastInternal eof) = getEOF eof
head (b :|* _) = getBodyToken b

-- |Obtain the tail of the list of Tokens. Since
--  Tokens must always end with an EOF, this will
--  always return a value.
tail :: Tokens -> Tokens
tail e@(TksLastInternal _) = e
tail (_ :|* rest) = rest

-- |Pattern matches `Tokens` when the only `Token` left
--  is EOF.
pattern TksLast :: Token -> Tokens
pattern TksLast t <- TksLastInternal (UnsafeEOFToken t)

-- |Patter matches `Tokens` when there is more than just
--  EOF left.
pattern (:|) :: Token -> Tokens -> Tokens
pattern t :| rest <- UnsafeBodyToken t :|* rest

-- Informs GHC that matching on (:|) and TksLast covers all cases of Tokens
{-# COMPLETE (:|), TksLast #-}

-- |Possibly constructs Tokens from a list of Token values.
--  Requires that the final object be an EOF, and none of
--  the preceding ones be EOF.
mkTokens :: [Token] -> Maybe Tokens
mkTokens []     = Nothing
mkTokens [t]    = TksLastInternal <$> mkEOF t
mkTokens (t:ts) = (:|*) <$> mkBody t <*> mkTokens ts
-- -----

-- |A value that an expression or variable represents when
--  evaluated. Expression "3 > 5" evalues to `VBoolean False`,
--  for example.
data Value =
    VString  String |
    VBoolean Bool   |
    VNumber  Double |
    VNil            |
    VCallable Int LoxCallable

instance Eq Value where
    VString s1 == VString s2 = s1 == s2
    VBoolean b1 == VBoolean b2 = b1 == b2
    VNumber d1 == VNumber d2 = d1 == d2
    VNil == VNil = True
    VCallable _ _ == VCallable _ _ = False
    _ == _ = False

instance Show Value where
    show (VString s)   = s
    show (VBoolean b)  = show b
    show (VNumber d)   =
        if isInteger d then show (truncate d :: Int64)
        else printf "%f" d
    show VNil          = "nil"
    show (VCallable _ lc) = 
        case lc of
            UserDefined decl _ -> "<fn " <> funName decl <> ">"
            NativeFunction _ -> "<fn native>"

-- |Quick math to confirm a Double is an integer
--  value. Only used to determine if the ".0" 
--  needs to be stripped from the end for printing.
isInteger :: Double -> Bool
isInteger d
    | isNaN d || isInfinite d   = False
    | abs d >= 9007199254740992 = True
    | otherwise                 = d == fromIntegral (truncate d :: Int64)

-- Functions!
-- |Represents a function, either defined by the user with `Lx_Fun` or
--  a native one defined by the language such as "clock()".
data LoxCallable =
    UserDefined FunctionDeclaration Locals |
    NativeFunction ([Value] -> IO (Either EvalError Value))

-- Statements!
-- |Represents a statement in Lox. For example, a `VarDeclaration` 
--  representing "var a = 5 > 3" would contain "a" as the `String`
--  and "5 > 3" as the `Expression`.
data Statement =
    -- TODO: should these declarations use Token values instead?
    FunDeclaration FunctionDeclaration                 |
    VarDeclaration String Expression                   |
    ExpressionStatement   Expression                   |
    PrintStatement        Expression                   |
    Block                [Statement]                   |
    IfStatement Expression Statement (Maybe Statement) |
    WhileStatement Expression Statement                |
    ReturnStatement Token Expression
    deriving (Show)

data FunctionDeclaration = FunctionDeclaration
    { funName   :: String
    , funParams :: [String]
    , funBody   :: Statement
    } deriving (Show)

-- |A list of `Statement`s to be executed in order. Constructed
--  when a Lox file is lexed and parsed.
type Program = [Statement]
    