module Main (main) where

import System.Environment (getArgs)
import qualified Data.List.NonEmpty as NE

main :: IO ()
main = do
    args <- getArgs
    let args' = NE.nonEmpty args

    case args' of
        Nothing -> runPrompt
        Just as -> case NE.length as of
            1 -> runFile (NE.head as)
            _ -> usage 

runPrompt :: IO ()
runPrompt = undefined

runFile :: FilePath -> IO ()
runFile = undefined

usage :: IO ()
usage = putStrLn "Usage: yahili [script]"