module Statement where

import Language
import Parse

import Prelude hiding (init)
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
    m <- match [Lx_Print,Lx_Var,Lx_LeftBrace]
    case m of
        Just t  
            | getTokenType t == Lx_If        -> parseIfStmt
            | getTokenType t == Lx_Print     -> parsePrintStmt
            | getTokenType t == Lx_Var       -> parseDeclaration
            | getTokenType t == Lx_LeftBrace -> parseBlock
        _ -> parseExpressionStmt

parseIfStmt :: Parsing Statement
parseIfStmt = do
    consume_ Lx_LeftParen "Expect '(' after 'if'."
    condition <- parseExpression
    consume_ Lx_RightParen "Expect ')' after if condition." 
    
    thenBranch <- parseStatement
    m <- match [Lx_Else]
    case m of
        Just _ -> do
            elseBranch <- parseStatement
            pure $ IfStatement condition thenBranch (Just elseBranch)
        Nothing -> pure $ IfStatement condition thenBranch Nothing

parseDeclaration :: Parsing Statement
parseDeclaration = do
    t <- consume Lx_Identifier "Expect variable name."
    val <- init
    consume_ Lx_Semicolon "Expect ';' after variable declaration."
    pure $ VarDeclaration (getLexeme t) val
  where
    init :: Parsing Expression
    init = do
        m <- match [Lx_Equal]
        case m of
            Just _ -> parseExpression
            Nothing -> pure LNil


parsePrintStmt :: Parsing Statement
parsePrintStmt = do
    expr <- parseExpression
    consume_ Lx_Semicolon "Expect ';' after value."
    pure $ PrintStatement expr

parseExpressionStmt :: Parsing Statement
parseExpressionStmt = do
    expr <- parseExpression
    consume_ Lx_Semicolon "Expect ';' after value."
    pure $ ExpressionStatement expr

parseBlock :: Parsing Statement
parseBlock = do
    statements <- go
    consume_ Lx_RightBrace "Expect '}' after block."
    pure $ Block statements
  where
    go :: Parsing [Statement]
    go = do
        next  <- peek
        ended <- atEnd
        if not (getTokenType next == Lx_RightBrace) && not ended then do
            stmt <- parseStatement
            (stmt :) <$> go
        else pure []
