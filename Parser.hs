-- vim:fdm=marker ts=4 sw=4 et

module Parser (
    parseProgram, parseBlock, parseDecl, parseBasicType, parseStmt,
    parseAssignment, parseIfStmt, parseIfElse, parseWhile, parseDoWhile,
    parseBreak, parseLoc, parseExpr, parseFactor{-, parseOperator-}
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Debug.Trace (trace)
import qualified Data.Map as Map

import Scanner
import Block
import Tac

type Compiler a = GenParser Char CompilerState a

test :: String -> Either ParseError CompilerState
test = runParser parseProgram newCompilerState ""

debug :: String -> Compiler ()
debug s = return $ trace (">" ++ s) ()

-- State helpers {{{
newLabel :: Compiler Label
newLabel = do
    l <- fmap getLabel getState
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
parseProgram = do
    whiteSpace
    b <- parseBlock
    eof
    getState

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
parseStmt = choice $ map try [
    parseAssignment,
    parseIfElse,
    parseIfStmt,
    parseWhile,
    parseDoWhile,
    parseBreak,
    semi >> return (),
    parseBlock]

parseAssignment = do
    l <- parseLoc
    reservedOp "="
    e <- parseExpr
    semi
    case l of
         ArrayIndex i [] -> codeAppend $
            Tac InstrAssign (Just e) Nothing (Just $ ArgID i)
         _ -> fail "Assigning to arrays is not yet supported"

parseIfStmt = do
    reserved "if"
    e <- parens parseExpr
    s <- parseStmt
    codeAppend noop

parseIfElse = do
    reserved "if"
    e <- parens parseExpr
    s <- parseStmt
    reserved "else"
    s' <- parseStmt
    codeAppend noop

parseWhile = do
    reserved "while"
    e <- parens parseExpr
    s <- parseStmt
    codeAppend noop

parseDoWhile = do
    reserved "do"
    s <- parseStmt
    reserved "while"
    e <- parens parseExpr
    semi
    codeAppend noop

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
parseExpr = try parseOperator <|> parseFactor

parseFactor :: Compiler Arg
parseFactor = choice $ map try [
    parens parseExpr,
    parseLocExpr,
    fmap ArgReal float,
    fmap ArgNum integer,
    (reserved "true" >> return (ArgNum 1)),
    (reserved "false" >> return (ArgNum 0))]

parseLocExpr = do
    l <- parseLoc
    case l of
         ArrayIndex i [] -> return $ ArgID i
         _ -> fail "Indeeeexing arrays is not yet supported"

parseOperator :: Compiler Arg
parseOperator = buildExpressionParser operatorTable parseFactor

binary :: String -> Op -> Operator Char CompilerState Arg
binary sym op = Infix (do
    reservedOp sym
    tmp <- newTemp BasicInt
    let t = ArgID tmp
    codeAppend $ Tac (InstrBinary op) (Just arg1) (Just arg2) (Just t)
    return $ \arg1 arg2 -> t
    ) AssocLeft

operatorTable = [[binary "+" OpAdd]]

{-
operatorTable = [
    [prefix "!" OpNot],
    [prefix "-" OpNeg],
    [binary "*" OpMul, binary "/" OpDiv],
    [binary "+" OpAdd, binary "-" OpSub],
    [binary "<" OpLT, binary ">" OpGT, binary "<=" OpLE, binary ">=" OpGE],
    [binary "==" OpEQ, binary "!=" OpNE],
    [binary "&&" OpAnd],
    [binary "||" OpOr]]
    where binary s f = Infix (reservedOp s >> return (BinExpr f) <?> "operator") AssocLeft
          prefix s f = Prefix (reservedOp s >> return (UnExpr f) <?>
              "operator")
-}
-- }}}
