module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P

import Program

lexicalRules = LanguageDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = True
    , identStart     = letter
    , identLetter    = alphaNum <|> char '_'
    , opStart = oneOf "=+-*/<>=!&|"
    , opLetter = opStart lexicalRules
    , reservedNames = words "if else do while break int float"
    , reservedOpNames = words "= + - * / < > == != <= >= && || !"
    , caseSensitive  = True
    }

lexer = P.makeTokenParser lexicalRules
braces     = P.braces lexer
identifier = P.identifier lexer
semiSep1   = P.semiSep1 lexer
semi       = P.semi lexer
squares    = P.squares lexer
symbol     = P.symbol lexer
whiteSpace = P.whiteSpace lexer

list unit = try (do { x <- unit;
                      xs <- list unit;
                      return $ x:xs
                      }) <|> return []

parseProgram = do whiteSpace
                  b <- parseBlock
                  eof
                  return $ Program b

parseBlock = braces $ do
    d <- list parseDecl
    s <- return [] --list parseStmt
    return $ Block d s

parseDecl = do
    b <- parseBasicType
    d <- list parseDimension
    i <- identifier
    semi
    return $ Decl (Type b d) i

parseBasicType = (symbol "int" >> return BasicInt)
    <|> (symbol "float" >> return BasicFloat)

parseDimension :: Parser TypeNum
parseDimension = squares $ read `fmap` many1 digit

parseStmt = undefined
