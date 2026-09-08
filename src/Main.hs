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
import Expression
import Language(mkTokens, Token (getLineNum, getTokenType, getLexeme), TokenType (Lx_EOF), Statement (..))
import Control.Monad.Writer (runWriter)
import qualified Data.Map as Map
import Statement

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
        exitCode <- run contents
        maybe (pure ()) (exitWith . ExitFailure) exitCode

usage :: IO ()
usage = putStrLn "Usage: yahili [script]"

run :: String -> IO (Maybe Int)
run source = do
    let scanned = tokensFromSource source

        (mtokens,scanErrors) = runWriter scanned 

    if (not . null) scanErrors then do
        forM_ scanErrors $ \err ->
            case err of
                UnexpectedChar     n c -> loxError n $ "Unexpected character:  " <> [c]
                UnterminatedString n   -> loxError n $ "Unterminated string starting on line " <> show n
                UnclosedComment    n   -> loxError n $ "Unclosed comment starting on line " <> show n
        pure $ Just 65
    else case mkTokens mtokens of
        Just tokens -> do
            let parsed = parseProgram tokens
                (p,parseErrors) = runWriter parsed
            if (not . null) parseErrors then do
                forM_ parseErrors $ \case
                    ParseError t message -> do
                        let ln = getLineNum t
                            lx = getLexeme  t
                        if getTokenType t == Lx_EOF then
                            loxReport ln " at end" message
                        else
                            loxReport ln (" at '" <> lx <> "'") message
                pure $ Just 65
            else do
                vars <- foldM interpret (Just Map.empty) p
                case vars of
                    Nothing -> pure $ Just 70
                    _       -> pure Nothing
        Nothing -> do
            putStrLn "The list of tokens does not end in EOF! How'd that happen?"
            pure $ Just 65

-- The meat.
interpret :: Maybe Variables -> Statement -> IO (Maybe Variables)
interpret mvars st = do
    case mvars of
        Nothing -> pure Nothing
        Just vars -> case st of
            PrintStatement e ->
                case evaluate vars e of
                    Left er -> do
                        runtimeError er
                        pure $ Just vars
                    Right (val,vars') -> do
                        putStrLn $ show val
                        pure $ Just vars'
            ExpressionStatement e ->
                case evaluate vars e of
                    Left er -> do
                        runtimeError er
                        pure $ Just vars
                    Right (_,vars') -> do
                        pure $ Just vars'


-- Error stuff.
loxError :: Int -> String -> IO ()
loxError lineNum message = loxReport lineNum "" message

loxReport :: Int -> String -> String -> IO ()
loxReport lineNum where' message = do
    hPutStrLn stderr $ "[line " <> show lineNum <> "] Error" <> where' <> ": " <> message

runtimeError :: EvalError -> IO ()
runtimeError (EvalError t msg) = do
    hPutStrLn stderr $ msg <> "\n[line " <> show (getLineNum t) <> "]"