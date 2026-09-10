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
    m <- match [Lx_Print,Lx_Var,Lx_LeftBrace,Lx_If,Lx_While,Lx_For]
    case m of
        Just t  
            | getTokenType t == Lx_If        -> parseIfStmt
            | getTokenType t == Lx_While     -> parseWhileStmt
            | getTokenType t == Lx_For       -> parseForStmt
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
    elseBranch <- case m of
        Just _ -> Just <$> parseStatement
        Nothing -> pure Nothing
    pure $ IfStatement condition thenBranch elseBranch

parseWhileStmt :: Parsing Statement
parseWhileStmt = do
    consume_ Lx_LeftParen "Expect '(' after 'while'."
    condition <- parseExpression
    consume_ Lx_RightParen "Expect ')' after condition." 
    body <- parseStatement
    pure $ WhileStatement condition body

parseForStmt :: Parsing Statement
parseForStmt = do
    consume_ Lx_LeftParen "Expect '(' after 'for'."
    minitializer <- parseInitializer
    mcondition   <- parseWithoutToken Lx_Semicolon
    consume_ Lx_Semicolon "Expect ';' after loop condition."
    mincrement   <- parseWithoutToken Lx_RightParen
    consume_ Lx_RightParen "Expect ')' after for clauses."
    addInitializer minitializer
        . addCondition mcondition
        . addIncrement mincrement
        <$> parseStatement
  where
    parseInitializer :: Parsing (Maybe Statement)
    parseInitializer = do
        m <- match [Lx_Semicolon,Lx_Var]
        case m of
            Just t
                | getTokenType t == Lx_Semicolon -> pure Nothing
                | getTokenType t == Lx_Var       -> Just <$> parseDeclaration
            _                                    -> Just <$> parseStatement
    parseWithoutToken :: TokenType -> Parsing (Maybe Expression)
    parseWithoutToken tt = do
        s <- peek
        if getTokenType s == tt then pure Nothing
        else Just <$> parseExpression
    addIncrement :: Maybe Expression -> Statement -> Statement
    addIncrement mex body =
        case mex of
            Nothing  -> body
            Just inc -> Block [body,ExpressionStatement inc]
    addCondition :: Maybe Expression -> Statement -> Statement
    addCondition mex body =
        case mex of
            Nothing ->  WhileStatement (LBoolean True) body
            Just con -> WhileStatement con body
    addInitializer :: Maybe Statement -> Statement -> Statement
    addInitializer mst body =
        case mst of
            Nothing -> body
            Just st -> Block [st,body]

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
