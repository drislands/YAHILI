{-# LANGUAGE LambdaCase #-}

module Scan where

import Data.Char (isDigit,isAlpha,isAlphaNum)
import Control.Monad.Writer
import qualified Data.List.NonEmpty as NE

import Language (Token(..),TokenType(..))

findKeyword :: String -> TokenType
findKeyword = \case
    "and"    -> Lx_And
    "class"  -> Lx_Class
    "else"   -> Lx_Else
    "false"  -> Lx_False
    "for"    -> Lx_For
    "fun"    -> Lx_Fun
    "if"     -> Lx_If
    "nil"    -> Lx_Nil
    "or"     -> Lx_Or
    "print"  -> Lx_Print
    "return" -> Lx_Return
    "super"  -> Lx_Super
    "this"   -> Lx_This
    "true"   -> Lx_True
    "var"    -> Lx_Var
    "while"  -> Lx_While
    _        -> Lx_Identifier

data LexError =
    UnexpectedChar Int Char |
    UnterminatedString Int  |
    UnclosedComment Int
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
    (scanned, n', ss) <- scanToken s
    case scanned of
        Nothing   -> tokensFromSource' (ts,n',ss)
        Just token-> tokensFromSource' (token:ts,n',ss)
  where
    scanToken :: String -> Lexing  (Maybe Token,Int,String)
    scanToken "" = pure (Nothing, n, "")
    scanToken (x:xs) = case x of
        -- Single-character tokens
        '(' -> tok Lx_LeftParen  [x] xs
        ')' -> tok Lx_RightParen [x] xs
        '{' -> tok Lx_LeftBrace  [x] xs
        '}' -> tok Lx_RightBrace [x] xs
        ',' -> tok Lx_Comma      [x] xs
        '.' -> tok Lx_Dot        [x] xs
        '-' -> tok Lx_Minus      [x] xs
        '+' -> tok Lx_Plus       [x] xs
        ';' -> tok Lx_Semicolon  [x] xs
        '*' -> tok Lx_Star       [x] xs

        -- Possibly-two-character tokens
        '!' -> match '=' Lx_BangEqual    Lx_Bang    xs
        '=' -> match '=' Lx_EqualEqual   Lx_Equal   xs
        '<' -> match '=' Lx_LessEqual    Lx_Less    xs
        '>' -> match '=' Lx_GreaterEqual Lx_Greater xs

        -- Comments, maybe!
        '/' -> case xs of
            '/':_ -> do
                let (_,rest) = break (\c-> c=='\n') xs
                pure (Nothing, n, rest)
            '*':rest -> do
                case closeComment n rest of
                    (n',"") -> do
                        tell [UnclosedComment n]
                        pure (Nothing, n', "")
                    (n',rest') -> pure (Nothing, n', rest')
            _     -> tok Lx_Slash [x] xs

        -- Newlines and whitespace!
        '\n' -> pure (Nothing, n+1, xs)
        ' '  -> pure (Nothing, n, xs)
        '\t' -> pure (Nothing, n, xs)
        '\r' -> pure (Nothing, n, xs)

        -- Literals and Identifiers!
        '"' -> makeString
        c | isDigit c -> makeNumber     c
          | isAlpha c -> makeIdentifier c

        -- Unexpected characters
        _   -> do
            tell [UnexpectedChar n x]
            pure (Nothing, n, xs)
      where
        tok :: TokenType -> String -> String -> Lexing (Maybe Token,Int,String)
        tok tt lexeme rest = pure (Just Token { getTokenType = tt, getLexeme = lexeme, getLineNum = n },n,rest)
        match :: Char -> TokenType -> TokenType -> String -> Lexing (Maybe Token,Int,String)
        match expected twoToken oneToken rest = case rest of
            c:cs | c == expected -> tok twoToken [x,c] cs
            _                    -> tok oneToken [x]   rest
        makeNumber :: Char -> Lexing (Maybe Token,Int,String)
        makeNumber c = do
            let (left,rest) = span isDigit xs
            case rest of
                -- If the next character is a period and the one after is a digit, we have a decimal number.
                '.' : rest'@(c':_) | isDigit c' -> do
                    let (right,rest'') = span isDigit rest'
                        value = [c] <> left <> "." <> right
                        token = Token { getTokenType = Lx_Number, getLexeme = value, getLineNum = n}
                    pure (Just token, n, rest'')
                -- Otherwise the original set of digits is it.
                _ -> do
                    let token = Token { getTokenType = Lx_Number, getLexeme = [c] <> left, getLineNum = n}
                    pure (Just token, n, rest)
        countNewlines :: String -> Int
        countNewlines = foldr (\c acc -> if c=='\n' then acc+1 else acc) 0 
        makeString :: Lexing (Maybe Token,Int,String)
        makeString = do
            let (value,rest) = break (=='"') xs
                newlineCount = countNewlines value
                endLine      = n+newlineCount
            case NE.nonEmpty rest of
                Nothing -> do
                    tell [UnterminatedString n]
                    pure (Nothing, endLine, rest)
                Just rest' -> do
                    -- get the tail here to skip the closing quote.
                    let token = Token { getTokenType = Lx_String, getLexeme = value, getLineNum = endLine }
                    pure (Just token, endLine, NE.tail rest')
        makeIdentifier :: Char -> Lexing (Maybe Token,Int,String)
        makeIdentifier c = do
            let (value',rest) = span isAlphaNum xs
                value         = [c] <> value'
                tt            = findKeyword value
                token = Token { getTokenType = tt, getLexeme = value, getLineNum = n }
            pure (Just token,n,rest)
        closeComment :: Int -> String -> (Int,String)
        closeComment n' rest = 
            let (left,right) = break (=='*') rest
                newlineCount = countNewlines left + n'
            in  case right of
                '*':'/':rest' -> (newlineCount,rest')
                '*':rest'     -> closeComment newlineCount rest'
                _             -> (newlineCount,"")