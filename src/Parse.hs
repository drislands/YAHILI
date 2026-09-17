{-# LANGUAGE LambdaCase #-}

module Parse where

import Language

import Prelude hiding (head,tail,init)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad (unless)
-- import GHC.Pre


data ParseError =
    ParseError Token String 
    deriving (Show)
type Parsing a = StateT Tokens (Writer [ParseError]) a

-- Expression parsing.
parseExpression :: Parsing Expression
parseExpression = parseAssignment

parseAssignment :: Parsing Expression
parseAssignment = do
    expr <- parseOr
    m <- match [Lx_Equal]
    case m of
        Nothing -> pure expr
        Just eq -> do
            case expr of
                Identifier v -> do
                    value <- parseAssignment
                    pure $ Assignment v value
                _            -> do
                    lift $ tell [ParseError eq "Invalid assignment target."]
                    pure expr

parseOr :: Parsing Expression
parseOr = do
    left <- parseAnd
    m <- match [Lx_Or]
    case m of
        Nothing -> pure left
        Just op  -> do
            right <- parseOr
            pure $ Logical left op right

parseAnd :: Parsing Expression
parseAnd = do
    left <- parseEquality
    m <- match [Lx_And]
    case m of
        Nothing -> pure left
        Just op  -> do
            right <- parseAnd
            pure $ Logical left op right

parseEquality :: Parsing Expression
parseEquality = parseBinary parseComparison [Lx_BangEqual,Lx_EqualEqual]

parseComparison :: Parsing Expression
parseComparison = parseBinary parseTerm [Lx_Greater,Lx_GreaterEqual,Lx_Less,Lx_LessEqual]

parseTerm :: Parsing Expression
parseTerm = parseBinary parseFactor [Lx_Minus,Lx_Plus]

parseFactor :: Parsing Expression
parseFactor = parseBinary parseUnary [Lx_Slash,Lx_Star]

parseBinary :: Parsing Expression -> [TokenType] -> Parsing Expression
parseBinary nextFunc types = do
    left <- nextFunc
    loop left
  where
    loop :: Expression -> Parsing Expression
    loop expr = do
        match types >>= \case
            Just t -> do
                right <- nextFunc
                loop (Binary expr t right)
            Nothing -> pure expr

parseUnary :: Parsing Expression
parseUnary = do
    match [Lx_Bang,Lx_Minus] >>= \case
        Just t -> do
            right <- parseUnary
            pure $ Unary t right
        Nothing -> parseCall

parseCall :: Parsing Expression
parseCall = do
    expr <- parsePrimary
    finishCall expr
  where
    finishCall :: Expression -> Parsing Expression
    finishCall callee = do
        m <- match [Lx_LeftParen]
        case m of
            Nothing -> pure callee
            Just _  -> do
                rp <- peek
                margs <- if getTokenType rp == Lx_RightParen
                    then pure $ Right []
                    else parseArgs 1
                case margs of
                    Left t -> do
                        lift $ tell [ParseError t "Can't have more than 255 arguments."]
                        synchronize
                        pure LNil
                    Right args -> do
                        paren <- consume Lx_RightParen "Expect ')' after arguments."
                        finishCall (Call callee paren args)

    parseArgs :: Int -> Parsing (Either Token [Expression])
    parseArgs n = do
        if n > 255 then do
            t <- peek
            pure $ Left t
        else do
            arg <- parseExpression
            m <- match [Lx_Comma]
            case m of
                Nothing -> pure $ Right [arg]
                Just _  -> (fmap . fmap) (arg :) (parseArgs (n+1))

parsePrimary :: Parsing Expression
parsePrimary = do
    t <- peek
    case matchLit t of
        Just e  -> do
            advance
            pure e
        Nothing -> do
            match [Lx_LeftParen] >>= \case
                Just _ -> do
                    e <- parseExpression
                    consume_ Lx_RightParen "Expect ')' after expression."
                    pure $ Grouping e
                Nothing -> do
                    e <- peek
                    lift $ tell [ParseError e "Expect expression."]
                    pure LNil

  where
    matchLit :: Token -> Maybe Expression
    matchLit t =
        let tt = getTokenType t
            tl = getLexeme    t
        in  case tt of
            Lx_False      -> Just (LBoolean False)
            Lx_True       -> Just (LBoolean True)
            Lx_Nil        -> Just (LNil)
            Lx_String     -> Just (LString tl)
            Lx_Number     -> Just (LNumber (read tl))
            Lx_Identifier -> Just (Identifier t)
            _             -> Nothing

-- Statement parsing.
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
    t <- peek
    case getTokenType t of
        Lx_If        -> advance >> parseIfStmt
        Lx_While     -> advance >> parseWhileStmt
        Lx_For       -> advance >> parseForStmt
        Lx_Print     -> advance >> parsePrintStmt
        Lx_Var       -> advance >> parseDeclaration
        Lx_LeftBrace -> advance >> parseBlock
        Lx_Fun       -> advance >> parseFunction "function"
        Lx_Return    -> advance >> parseReturnStmt t
        _            -> parseExpressionStmt

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

parseFunction :: String -> Parsing Statement
parseFunction kind = do
    name' <- consume Lx_Identifier ("Expect " <> kind <> " name.")
    let name = getLexeme name'
    consume_ Lx_LeftParen ("Expect '(' after " <> kind <> " name.")
    p <- peek 
    mparams <- case getTokenType p of
        Lx_RightParen -> pure $ Right []
        _             -> parseParameters 1
    case mparams of
        Left t -> do
            lift $ tell [ParseError t "Can't have more than 255 parameters."]
            synchronize
            pure $ Block []
        Right params' -> do
            let params = map getLexeme params'
            consume_ Lx_RightParen "Expect ')' after parameters."
            consume_ Lx_LeftBrace  ("Expect '{' before " <> kind <> " body.")
            body <- parseBlock
            pure $ FunDeclaration $ 
                FunctionDeclaration 
                { funName = name
                , funParams = params
                , funBody = body
                }
  where
    parseParameters :: Int -> Parsing (Either Token [Token])
    parseParameters n = do
        if n > 255 then do
            t <- peek
            pure $ Left t
        else do
            param <- consume Lx_Identifier "Expect parameter name."
            m <- match [Lx_Comma]
            case m of
                Nothing -> pure $ Right [param]
                Just _  -> (fmap . fmap) (param :) (parseParameters (n+1))

parseReturnStmt :: Token -> Parsing Statement
parseReturnStmt t = do
    next <- peek
    value <- if getTokenType next == Lx_Semicolon then pure LNil
        else parseExpression
    consume_ Lx_Semicolon "Expect ';' after return value."
    pure $ ReturnStatement t value

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

-- Helper stateful functions.
match :: [TokenType] -> Parsing (Maybe Token)
match types = do
    t <- peek
    if getTokenType t `elem` types then do
        advance
        pure (Just t)
    else pure Nothing

advance :: Parsing ()
advance = do
    rest <- get
    put $ tail rest

atEnd :: Parsing Bool
atEnd = do
    ts <- get
    case ts of
        TksLast _ -> pure True
        _         -> pure False

peek :: Parsing Token
peek = do
    tokens <- get
    pure $ head tokens

-- Error handling!
consume :: TokenType -> String -> Parsing Token
consume tt message = do
    matched <- match [tt]
    case matched of
        Just good -> pure good
        Nothing -> do
            bad <- peek
            lift $ tell [ParseError bad message]
            synchronize
            pure bad

consume_ :: TokenType -> String -> Parsing ()
consume_ tt message = do
    _ <- consume tt message
    pure ()

synchronize :: Parsing ()
synchronize = do
    advance
    t <- peek
    unless (statementTerm t) synchronize
  where
    statementTerm :: Token -> Bool
    statementTerm t = getTokenType t `elem` 
        [ Lx_Class
        , Lx_Fun
        , Lx_Var
        , Lx_If
        , Lx_While
        , Lx_Print
        , Lx_Return
        , Lx_Semicolon
        , Lx_EOF
        ]

-- Other helper functions.
parenthesize :: String -> [Expression] -> String
parenthesize name es =
    "(" <> unwords (name : map uglyPrint es) <> ")"

uglyPrint :: Expression -> String
uglyPrint = \case
    Binary   left op right -> parenthesize (getLexeme op) [left,right]
    Grouping e             -> parenthesize "group" [e]
    Unary    op e          -> parenthesize (getLexeme op) [e]
    LNil                   -> "nil"
    LString  s             -> s
    LBoolean b             -> show b 
    LNumber  n             -> show n
    Assignment t e         -> parenthesize (getLexeme t <> "=") [e]
    Identifier t           -> getLexeme t
    Logical l op r         -> parenthesize (getLexeme op) [l,r]
    Call c _ args          -> parenthesize "call" (c:args)
    LambdaExpression _     -> ""