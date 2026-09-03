{-# LANGUAGE LambdaCase #-}

module Parse where

import Language
import Scan

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