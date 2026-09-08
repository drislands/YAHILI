{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.IO
import System.Environment (getArgs)
import System.Directory
import qualified Data.List.NonEmpty as NE
import Control.Monad
import System.Exit (exitWith, ExitCode (ExitFailure))
import Scan (tokensFromSource, LexError (..))
import Parse
import Evaluate
import Language(mkTokens, Token (getLineNum, getTokenType, getLexeme), TokenType (Lx_EOF))
import Control.Monad.Writer (runWriter)

main :: IO ()
main = do
    args <- getArgs
    let args' = NE.nonEmpty args

    case args' of
        Nothing -> runPrompt
        Just as -> case NE.length as of
            1 -> runFile (NE.head as)
            _ -> do 
                usage 
                exitWith (ExitFailure 64)

runPrompt :: IO ()
runPrompt = do
    putStr " > "
    hFlush stdout
    done <- isEOF
    unless done $ do
        input <- getLine
        _ <- run input
        runPrompt

runFile :: FilePath -> IO ()
runFile file = do
    exists <- doesFileExist file
    if not exists then usage else do
        handle <- openFile file ReadMode
        contents <- hGetContents handle
        succeeded <- run contents
        unless succeeded $ exitWith (ExitFailure 65)

usage :: IO ()
usage = putStrLn "Usage: yahili [script]"

run :: String -> IO Bool
run source = do
    let scanned = tokensFromSource source

        (mtokens,scanErrors) = runWriter scanned 

    if (not . null) scanErrors then do
        forM_ scanErrors $ \err ->
            case err of
                UnexpectedChar     n c -> loxError n $ "Unexpected character:  " <> [c]
                UnterminatedString n   -> loxError n $ "Unterminated string starting on line " <> show n
                UnclosedComment    n   -> loxError n $ "Unclosed comment starting on line " <> show n
        pure False
    else case mkTokens mtokens of
        Just tokens -> do
            let parsed = parse tokens
                (e,parseErrors) = runWriter parsed
            if (not . null) parseErrors then do
                forM_ parseErrors $ \case
                    ParseError t message -> do
                        let ln = getLineNum t
                            lx = getLexeme  t
                        if getTokenType t == Lx_EOF then
                            loxReport ln " at end" message
                        else
                            loxReport ln (" at '" <> lx <> "'") message

                pure False
            else do
                case evaluate e of
                    Left (EvalError t msg) -> do
                        let ln = getLineNum t
                        loxError ln msg
                        pure False
                    Right (val,_) -> do
                        putStrLn $ "Result: " <> show val
                        pure True
        Nothing -> do
            putStrLn "The list of tokens does not end in EOF! How'd that happen?"
            pure False


-- Error stuff.
loxError :: Int -> String -> IO ()
loxError lineNum message = loxReport lineNum "" message

loxReport :: Int -> String -> String -> IO ()
loxReport lineNum where' message = do
    hPutStrLn stderr $ "[line " <> show lineNum <> "] Error" <> where' <> ": " <> message