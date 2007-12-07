module Parser (
    -- * Helper functions
    test,
    debug,
    choiceTry,
    newLabel,
    newTemp,
    beginBlock,
    endBlock,
    codeAppend,
    -- * Parsers
    -- ** Program structure
    parseProgram,
    parseBlock,
    parseDecl,
    parseBasicType,
    -- ** Statements
    parseStmt,
    parseAssignment,
    parseIfStmt,
    parseIfElse,
    parseWhile,
    parseDoWhile,
    parseBreak,
    -- ** Expressions
    parseLoc,
    parseExpr,
    parseOr, parseAnd, parseEq, parseRel, parseAdd, parseProd, parseUnary,
    unary,
    binary,
    parseFactor,
    parseLocExpr,
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Debug.Trace (trace)
import qualified Data.Map as Map

import Scanner
import Block
import Tac

type Compiler a = GenParser Char CompilerState a

-- |Calls 'parseProgram', the main entry point for the compiler, with a
-- new 'CompilerState'.
test :: String -> Either ParseError CompilerState
test = runParser parseProgram newCompilerState ""

debug :: String -> Compiler ()
debug s = return $ trace ("> " ++ s) ()

-- State helpers {{{
choiceTry :: [Compiler a] -> Compiler a
choiceTry = choice . map try

newLabel :: Compiler Label
newLabel = do
    l <- fmap nextLabel getState
    updateState addLabel
    return l

newTemp :: BasicType -> Compiler ID
newTemp bt = do
    l <- fmap getTemp getState
    updateState . addTemp $ bt
    return l

beginBlock :: Compiler Env
beginBlock = do
    updateState openBlock
    fmap locals getState

endBlock :: Compiler ()
endBlock = do
    s <- getState
    debug $ "Closing block " ++ show (blocks s Map.! locals s)
    updateState closeBlock

codeAppend :: Tac -> Compiler ()
codeAppend c = do
    s <- getState
    setState s { code = code s ++ [c] }

-- }}}

-- Blocks {{{
parseProgram :: Compiler CompilerState
parseProgram = do
    whiteSpace
    b <- parseBlock
    eof
    getState

parseBlock :: Compiler ()
parseBlock = braces $ do
    e <- beginBlock
    debug $ "Env " ++ show e
    d <- many parseDecl
    s <- many parseStmt
    endBlock
-- }}}

-- Declarations {{{
parseDecl :: Compiler ()
parseDecl = do
    b <- parseBasicType
    d <- many $ squares natural
    i <- identifier
    semi
    updateState $ addDecl i False (Type b d)

parseBasicType :: Compiler BasicType
parseBasicType = (reserved "int" >> return BasicInt)
    <|> (reserved "float" >> return BasicFloat)
-- }}}

-- Statements {{{
parseStmt :: Compiler ()
parseStmt = choiceTry [
    parseAssignment,
    parseIfElse,
    parseIfStmt,
    parseWhile,
    parseDoWhile,
    parseBreak,
    semi >> return (),
    parseBlock]

parseAssignment :: Compiler ()
parseAssignment = do
    l <- parseLoc
    reservedOp "="
    e <- parseExpr
    semi
    case l of
         ArrayIndex i [] -> codeAppend $
            Tac InstrAssign (Just e) Nothing (Just $ ArgID i)
         _ -> fail "Assigning to arrays is not yet supported"

parseIfStmt :: Compiler ()
parseIfStmt = do
    reserved "if"
    e <- parens parseExpr
    s <- parseStmt
    codeAppend noop

parseIfElse :: Compiler ()
parseIfElse = do
    reserved "if"
    e <- parens parseExpr
    s <- parseStmt
    reserved "else"
    s' <- parseStmt
    codeAppend noop

parseWhile :: Compiler ()
parseWhile = do
    reserved "while"
    e <- parens parseExpr
    s <- parseStmt
    codeAppend noop

parseDoWhile :: Compiler ()
parseDoWhile = do
    reserved "do"
    s <- parseStmt
    reserved "while"
    e <- parens parseExpr
    semi
    codeAppend noop

parseBreak :: Compiler ()
parseBreak = do
    reserved "break"
    semi
    codeAppend noop
-- }}}

-- Expressions {{{
parseLoc :: Compiler ArrayIndex
parseLoc = do
    i <- identifier
    es <- many $ brackets parseExpr
    return $ ArrayIndex i es

parseExpr :: Compiler Arg
parseExpr = parseOr

parseOr, parseAnd, parseEq, parseRel, parseAdd, parseProd, parseUnary
    :: Compiler Arg

parseOr = binary "||" OpOr parseAnd
parseAnd = binary "&&" OpAnd parseEq
parseEq = choiceTry $ g parseRel [binary "==" OpEQ, binary "!=" OpNE]
parseRel = choiceTry $ g parseAdd [binary "<=" OpLE, binary "<" OpLT,
    binary ">=" OpGT, binary ">" OpGT]
parseAdd = choiceTry $ g parseProd [binary "+" OpAdd, binary "-" OpSub]
parseProd = choiceTry $ g parseUnary [binary "*" OpMul, binary "/" OpDiv]
parseUnary = choiceTry $ g parseFactor [unary "!" OpNot, unary "-" OpNeg]
g = map . flip ($)

unary :: String -> Op -> Compiler Arg -> Compiler Arg
unary op opcode base = (do
    reservedOp op
    a1 <- base
    tmp <- newTemp BasicInt
    let t = ArgID tmp
    codeAppend $ Tac (InstrUnary opcode) (Just a1) Nothing (Just t)
    return t)
  <|> base

binary :: String -> Op -> Compiler Arg -> Compiler Arg
binary op opcode base = (try $ do
    a1 <- base
    reservedOp op
    a2 <- parseFactor
    tmp <- newTemp BasicInt
    let t = ArgID tmp
    codeAppend $ Tac (InstrBinary opcode) (Just a1) (Just a2) (Just t)
    return t)
 <|> base

parseFactor :: Compiler Arg
parseFactor = choiceTry [
    parens parseExpr,
    parseLocExpr,
    fmap ArgReal float,
    fmap ArgNum integer,
    (reserved "true" >> return (ArgNum 1)),
    (reserved "false" >> return (ArgNum 0))]

parseLocExpr :: Compiler Arg
parseLocExpr = do
    l <- parseLoc
    case l of
         ArrayIndex i [] -> return $ ArgID i
         _ -> fail "Indexing arrays is not yet supported"

-- }}}
