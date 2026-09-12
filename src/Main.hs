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
import Language
import Control.Monad.Writer (runWriter)
import qualified Data.Map as Map
import Statement
import Data.Bifunctor (first)
import Function (mkGlobalFunctions)

main :: IO ()
main = do
    args <- getArgs
    let args' = NE.nonEmpty args

    case args' of
        Nothing -> runPrompt mkGlobalFunctions
        Just as -> case NE.length as of
            1 -> runFile (NE.head as)
            _ -> do 
                usage 
                exitWith (ExitFailure 64)

runPrompt :: Variables -> IO ()
runPrompt vars = do
    putStr " > "
    hFlush stdout
    done <- isEOF
    unless done $ do
        input <- getLine
        result <- run vars input
        case result of
            Left _      -> runPrompt vars
            Right vars' -> runPrompt vars'

runFile :: FilePath -> IO ()
runFile file = do
    exists <- doesFileExist file
    if not exists then usage else do
        handle <- openFile file ReadMode
        contents <- hGetContents handle
        results <- run mkGlobalFunctions contents
        case results of
            Left exitCode -> (exitWith . ExitFailure) exitCode
            _             -> pure ()

usage :: IO ()
usage = putStrLn "Usage: yahili [script]"

run :: Variables -> String -> IO (Either Int Variables)
run vars source = do
    let scanned = tokensFromSource source

        (mtokens,scanErrors) = runWriter scanned 

    if (not . null) scanErrors then do
        forM_ scanErrors $ \err ->
            case err of
                UnexpectedChar     n c -> loxError n $ "Unexpected character:  " <> [c]
                UnterminatedString n   -> loxError n $ "Unterminated string starting on line " <> show n
                UnclosedComment    n   -> loxError n $ "Unclosed comment starting on line " <> show n
        pure $ Left 65
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
                pure $ Left 65
            else do
                mvars <- foldM interpret (Just vars) p
                case mvars of
                    Nothing    -> pure $ Left 70
                    Just vars' -> pure $ Right vars'
        Nothing -> do
            putStrLn "The list of tokens does not end in EOF! How'd that happen?"
            pure $ Left 65

-- The meat.
interpret :: Maybe Variables -> Statement -> IO (Maybe Variables)
interpret mvars st = do
    case mvars of
        Nothing -> pure Nothing
        Just vars -> case st of
            VarDeclaration name e -> do
                evaluate vars e >>= \case
                    Left er -> do
                        runtimeError er
                        pure Nothing
                    Right (val,vars') -> do
                        pure $ Just (define name val vars')
            PrintStatement e ->
                evaluate vars e >>= \case
                    Left er -> do
                        runtimeError er
                        pure Nothing
                    Right (val,vars') -> do
                        putStrLn $ show val
                        pure $ Just vars'
            ExpressionStatement e ->
                evaluate vars e >>= \case
                    Left er -> do
                        runtimeError er
                        pure Nothing
                    Right (_,vars') -> do
                        pure $ Just vars'
            Block statements -> do
                mvars' <- foldM interpret ((first (Map.empty :)) <$> mvars) statements
                pure $ case mvars' of
                    Just (_ : rest,g) -> Just (rest,g)
                    _                 -> Nothing
            IfStatement condition thenBranch elseBranch -> do
                evaluate vars condition >>= \case
                    Left er -> do
                        runtimeError er
                        pure Nothing
                    Right (val,vars') -> do
                        if truthy val then interpret (Just vars') thenBranch
                        else case elseBranch of
                            Just elseBranch' -> interpret (Just vars') elseBranch'
                            Nothing          -> pure $ Just vars'
            WhileStatement condition body -> while vars condition body
  where
    while :: Variables -> Expression -> Statement -> IO (Maybe Variables)
    while vars cond body = do
        evaluate vars cond >>= \case
            Left er -> do
                runtimeError er
                pure Nothing
            Right (val,vars') -> 
                if truthy val then do
                    mvars'' <- interpret (Just vars') body 
                    case mvars'' of
                        Nothing -> pure Nothing
                        Just vars'' -> while vars'' cond body
                else pure $ Just vars'


-- Error stuff.
loxError :: Int -> String -> IO ()
loxError lineNum message = loxReport lineNum "" message

loxReport :: Int -> String -> String -> IO ()
loxReport lineNum where' message = do
    hPutStrLn stderr $ "[line " <> show lineNum <> "] Error" <> where' <> ": " <> message

runtimeError :: EvalError -> IO ()
runtimeError (EvalError t msg) = do
    hPutStrLn stderr $ msg <> "\n[line " <> show (getLineNum t) <> "]"