module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer

import Scan
import Language
import Parse

-- Equality instances for testing.
instance Eq ParseError where
    ParseError t1 m1 == ParseError t2 m2 = 
        (getLexeme t1) == (getLexeme t2) && (m1 == m2)

instance Eq Token where
    t1 == t2 =
        let tt1 = getTokenType t1
            tt2 = getTokenType t2
            tl1 = getLexeme    t1
            tl2 = getLexeme    t2
        in (tt1 == tt2) && (tl1 == tl2)

instance Eq Expression where
    -- Literals
    LString  s1 == LString  s2 = s1 == s2
    LNumber  n1 == LNumber  n2 = n1 == n2
    LBoolean b1 == LBoolean b2 = b1 == b2
    LNil == LNil = True
    -- Variables/Functions
    Assignment t1 e1 == Assignment t2 e2 = (t1 == t2) && (e1 == e2)
    Identifier t1    == Identifier t2    =  t1 == t2
    Call e1 t1 es1   == Call e2 t2 es2   = (e1 == e2) && (t1 == t2)
        && (es1 == es2)
    -- Recursive
    Binary l1 t1 r1  == Binary l2 t2 r2 = (l1 == l2) && (t1 == t2)
        && (r1 == r2)
    Grouping e1      == Grouping e2     = e1 == e2
    Unary t1 e1      == Unary t2 e2     = (t1 == t2) && (e1 == e2)
    Logical l1 t1 r1 == Logical l2 t2 r2 = (l1 == l2) && (t1 == t2)
        && (r1 == r2)
    -- Anything else
    _ == _ = False


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Full Tests"
    [ testGroup "Scan Tests"
        [ testCase "Keywords vs Identifiers" $ do
            findKeyword "if" @?= Lx_If
            findKeyword "a"  @?= Lx_Identifier
        
        , testCase "Basic Token Scanning" $ do
            let input = "var x = 42;"
                (tokens, errors) = runWriter (tokensFromSource input)
            errors @?= []
            map getTokenType tokens @?= 
                [ Lx_Var
                , Lx_Identifier
                , Lx_Equal
                , Lx_Number
                , Lx_Semicolon
                , Lx_EOF
                ]

        , testCase "Unterminated String Error" $ do
            let input = "\"hello"
                (_,errors) = runWriter (tokensFromSource input)
            errors @?= [UnterminatedString 1]
        ]
    , testGroup "Parse Tests"
        [ testCase "Single Binary Parse" $ do
            let input = "3 + 5"
                expected = Binary (LNumber 3) 
                        (quickToken "+" Lx_Plus)
                        (LNumber 5)
            testExpression input expected
        , testCase "Assignment Parse" $ do
            let input = "v = (a and b)"
                expected = Assignment 
                    (quickToken "v" Lx_Identifier) (Grouping
                    (Logical 
                        (Identifier (quickToken "a" Lx_Identifier))
                        (quickToken "and" Lx_And)
                        (Identifier (quickToken "b" Lx_Identifier))))
            testExpression input expected
        , testCase "Complex Parse" $ do
            let input = "(17 + b) > (18 + -5 * 22)"
                expected = Binary
                    (Grouping (Binary 
                        (LNumber 17) 
                        (quickToken "+" Lx_Plus) 
                        (Identifier (quickToken "b" Lx_Identifier))))
                    (quickToken ">" Lx_Greater)
                    (Grouping (Binary
                        (LNumber 18)
                        (quickToken "+" Lx_Plus)
                        (Binary
                            (Unary 
                                (quickToken "-" Lx_Minus)
                                (LNumber 5))
                            (quickToken "*" Lx_Star)
                            (LNumber 22))))
            testExpression input expected
        ]
    ]

testExpression :: String -> Expression -> Assertion
testExpression input expected = do
    let (mtokens,errors) = runWriter (tokensFromSource input)
    errors @?= []
    case mkTokens mtokens of
        Nothing -> assertFailure $ "Tokens from `" <> input 
            <> "` could not be parsed"
        Just tokens -> do
            let (expression,perrors) = runWriter (parse tokens)
            perrors @?= []
            expression @?= expected

quickToken :: String -> TokenType -> Token
quickToken lexeme tt = Token 
    { getLexeme = lexeme
    , getTokenType = tt
    , getLineNum = 1
    }