module Main (main) where

import System.IO
import System.Environment (getArgs)
import System.Directory
import qualified Data.List.NonEmpty as NE
import Control.Monad
import System.Exit (exitWith, ExitCode (ExitFailure))
import Scan (tokensFromSource, LexError (..))
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
    let parsed = tokensFromSource source

        (tokens,errors) = runWriter parsed 

    if (not . null) errors then do
        forM_ errors $ \err ->
            case err of
                UnexpectedChar     n c -> loxError n $ "Unexpected character:  " <> [c]
                UnterminatedString n   -> loxError n $ "Unterminated string starting on line " <> show n
        pure False
    else do
        mapM_ (putStrLn . show) tokens
        pure True


-- Error stuff.
loxError :: Int -> String -> IO ()
loxError lineNum message = loxReport lineNum "" message

loxReport :: Int -> String -> String -> IO ()
loxReport lineNum where' message = do
    hPutStrLn stderr $ "[line " <> show lineNum <> "] Error" <> where' <> ": " <> message