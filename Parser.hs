module Parser where

import Text.ParserCombinators.Parsec

import Program

type PrgParser = GenParser Token PrgState Program

program :: Parser Program
program = do b <- block
             eof
             return $ Program b

block :: Parser Block
block = between (char '{') (char '}') $ do
    spaces
    d <- decls
    spaces
    s <- stmts
    spaces
    return $ Block d s

decls :: Parser [Decl]
decls = try $ do d <- decl
                 ds <- decls
                 return (d:ds)
    <|> return []

decl :: Parser Decl
decl = do t <- typep
          spaces
          i <- idp
          spaces
          char ';'
          return $ Decl t i

typep :: Parser Type
typep = do b <- basic
           d <- try (spaces >> dimension) `sepBy` spaces
           return $ Type b d

dimension :: Parser Dimension
dimension = between (char '[') (char ']') num

basic :: Parser BasicType
basic = do string "int"; return BasicInt
    <|> do string "float"; return BasicFloat

stmts :: Parser [Stmt]
stmts = return []

num :: Parser TypeNum
num = fmap read $ many1 digit

idp :: Parser ID
idp = do c <- letter
         s <- many $ alphaNum <|> char '_'
         return (c:s)
