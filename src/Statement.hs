module Statement where

import Language
import Parse
import Control.Monad.State
import Control.Monad.Writer (Writer)

parseProgram :: Tokens -> Writer [ParseError] Program
parseProgram = evalStateT parseProgram'


parseProgram' :: Parsing Program
parseProgram' = parseProgram'' []
    

parseProgram'' :: Program -> Parsing Program
parseProgram'' program = do
    tks <- get
    case tks of
        TksLast _ -> pure program
        _         -> do
            stmt <- parseStatement
            parseProgram'' $ program <> [stmt]

parseStatement :: Parsing Statement
parseStatement = do
    m <- match [Lx_Print,Lx_Var]
    case m of
        Just t  
            | getTokenType t == Lx_Print -> parsePrintStmt
            | getTokenType t == Lx_Var   -> parseDeclaration
        _ -> parseExpressionStmt

parseDeclaration :: Parsing Statement
parseDeclaration = do
    undefined
    -- t <- consume Lx_Identifier "Expect variable name."
    -- pure $ VarDeclaration (getLexeme t)

parsePrintStmt :: Parsing Statement
parsePrintStmt = do
    expr <- parseExpression
    consume Lx_Semicolon "Expect ';' after value."
    pure $ PrintStatement expr

parseExpressionStmt :: Parsing Statement
parseExpressionStmt = do
    expr <- parseExpression
    consume Lx_Semicolon "Expect ';' after value."
    pure $ ExpressionStatement expr