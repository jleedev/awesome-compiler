module Parser where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token

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

lexer = makeTokenParser lexicalRules

parseProgram = do whiteSpace lexer
                  b <- parseBlock
                  eof
                  return $ Program b

parseBlock = braces lexer $ do
    d <- many parseDecl
    s <- return [] --many parseStmt
    return $ Block d s

parseDecl = do
    b <- parseBasicType
    d <- many (squares lexer $ natural lexer)
    i <- identifier lexer
    semi lexer
    return $ Decl (Type b d) i

parseBasicType = (symbol lexer "int" >> return BasicInt)
    <|> (symbol lexer "float" >> return BasicFloat)

parseStmt = undefined
