module Language where

import Scan

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
    -- TODO: Variables?