module Main (main) where

import System.IO
import System.Environment (getArgs)
import System.Directory
import qualified Data.List.NonEmpty as NE
import Control.Monad
import System.Exit (exitWith, ExitCode (ExitFailure))
import Scan (tokensFromSource, LexError (..))
import Parse (parse)
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

        (tokens,scanErrors) = runWriter scanned 

    if (not . null) scanErrors then do
        forM_ scanErrors $ \err ->
            case err of
                UnexpectedChar     n c -> loxError n $ "Unexpected character:  " <> [c]
                UnterminatedString n   -> loxError n $ "Unterminated string starting on line " <> show n
                UnclosedComment    n   -> loxError n $ "Unclosed comment starting on line " <> show n
        pure False
    else do
        let parsed = parse tokens
            (exp,parseErrors) = runWriter parsed
        if (not . null) parseErrors then do
            putStrLn "Whoops!"
            pure False
        else do
            putStrLn $ show exp
            pure True


-- Error stuff.
loxError :: Int -> String -> IO ()
loxError lineNum message = loxReport lineNum "" message

loxReport :: Int -> String -> String -> IO ()
loxReport lineNum where' message = do
    hPutStrLn stderr $ "[line " <> show lineNum <> "] Error" <> where' <> ": " <> message