module Main (main) where

import System.IO
import System.Environment (getArgs)
import System.Directory
import qualified Data.List.NonEmpty as NE
import Control.Monad
import System.Exit (exitWith, ExitCode (ExitFailure))
import Scan (tokensFromSource)

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
        run input
        runPrompt

runFile :: FilePath -> IO ()
runFile file = do
    exists <- doesFileExist file
    if not exists then usage else do
        handle <- openFile file ReadMode
        contents <- hGetContents handle
        run contents

usage :: IO ()
usage = putStrLn "Usage: yahili [script]"

run :: String -> IO ()
run source = do
    let tokens = tokensFromSource source

    mapM_ (putStrLn . show) tokens