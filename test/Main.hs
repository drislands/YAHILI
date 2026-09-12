module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.Writer

import Scan
import Language


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Scan Tests"
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