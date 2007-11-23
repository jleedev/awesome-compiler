-- vim:fdm=marker ts=4 sw=4 et

module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

import Program
import Scanner

-- Blocks {{{
parseProgram = do whiteSpace
                  b <- parseBlock
                  eof
                  return $ Program b

parseBlock = braces $ do
    d <- many parseDecl
    s <- many parseStmt
    return $ Block d s
-- }}}

-- Declarations {{{
parseDecl = do
    b <- parseBasicType
    d <- many $ squares natural
    i <- identifier
    semi
    return $ Decl (Type b d) i

parseBasicType = (reserved "int" >> return BasicInt)
    <|> (reserved "float" >> return BasicFloat)
-- }}}

-- Statements {{{
parseStmt = choice $ map try [ parseAssignment
                             , parseIfElse
                             , parseIfStmt
                             , parseWhile
                             , parseDoWhile
                             , parseBreak
                             , semi >> return StmtEmpty
                             , fmap StmtBlock parseBlock]

parseAssignment = do
    l <- parseLoc
    reservedOp "="
    e <- parseExpr
    semi
    return $ StmtAssign l e

parseIfStmt = do
    reserved "if"
    e <- parens parseExpr
    s <- parseStmt
    return $ StmtIf e s

parseIfElse = do
    StmtIf e s <- parseIfStmt
    reserved "else"
    s' <- parseStmt
    return $ StmtIfElse e s s'

parseWhile = do
    reserved "while"
    e <- parens parseExpr
    s <- parseStmt
    return $ StmtWhile e s

parseDoWhile = do
    reserved "do"
    s <- parseStmt
    reserved "while"
    e <- parens parseExpr
    semi
    return $ StmtDoWhile s e

parseBreak = do
    reserved "break"
    semi
    return StmtBreak
-- }}}

-- Expressions {{{
parseLoc = do
    i <- identifier
    es <- many $ brackets parseExpr
    return $ LocIndex i es

parseExpr = try parseOperator <|> parseFactor

parseFactor = choice $ map try [ parens parseExpr
                               , fmap LocExpr parseLoc
                               , fmap LitReal float
                               , fmap LitNum integer
                               , (reserved "true" >> return (LitBool True))
                               , (reserved "false" >> return (LitBool False))
                               ]

parseOperator = buildExpressionParser operatorTable parseFactor

operatorTable = [ [prefix "!" OpNot]
                , [prefix "-" OpNeg]
                , [binary "*" OpMul, binary "/" OpDiv]
                , [binary "+" OpAdd, binary "-" OpSub]
                , [binary "<" OpLT, binary ">" OpGT,
                   binary "<=" OpLE, binary ">=" OpGE]
                , [binary "==" OpEQ, binary "!=" OpNE]
                , [binary "&&" OpAnd]
                , [binary "||" OpOr]]
    where binary s f = Infix (reservedOp s >> return (BinExpr f) <?>
              "operator") AssocLeft
          prefix s f = Prefix (reservedOp s >> return (UnExpr f) <?>
              "operator")

-- }}}
