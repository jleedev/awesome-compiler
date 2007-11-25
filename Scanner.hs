module Scanner (
    lexicalRules, lexer, identifier, reserved, operator, reservedOp,
    charLiteral, stringLiteral, natural, integer, float, naturalOrFloat,
    decimal, hexadecimal, octal, symbol, lexeme, whiteSpace, parens, braces,
    angles, brackets, squares, semi, comma, colon, dot, semiSep, semiSep1,
    commaSep, commaSep1,
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as T

lexicalRules = LanguageDef
    { commentStart   = "/*"
    , commentEnd     = "*/"
    , commentLine    = "//"
    , nestedComments = True
    , identStart     = letter
    , identLetter    = alphaNum <|> char '_'
    , opStart = oneOf "=+-*/<>=!&|"
    , opLetter = opStart lexicalRules
    , reservedNames = words "if else do while break int float true false"
    , reservedOpNames = words "+ - * / < > == != <= >= && || !"
    , caseSensitive  = True
    }

lexer = T.makeTokenParser lexicalRules

identifier     = T.identifier     lexer
reserved       = T.reserved       lexer
operator       = T.operator       lexer
reservedOp     = T.reservedOp     lexer
charLiteral    = T.charLiteral    lexer
stringLiteral  = T.stringLiteral  lexer
natural        = T.natural        lexer
integer        = T.integer        lexer
float          = T.float          lexer
naturalOrFloat = T.naturalOrFloat lexer
decimal        = T.decimal        lexer
hexadecimal    = T.hexadecimal    lexer
octal          = T.octal          lexer
symbol         = T.symbol         lexer
lexeme         = T.lexeme         lexer
whiteSpace     = T.whiteSpace     lexer
parens         = T.parens         lexer
braces         = T.braces         lexer
angles         = T.angles         lexer
brackets       = T.brackets       lexer
squares        = T.squares        lexer
semi           = T.semi           lexer
comma          = T.comma          lexer
colon          = T.colon          lexer
dot            = T.dot            lexer
semiSep        = T.semiSep        lexer
semiSep1       = T.semiSep1       lexer
commaSep       = T.commaSep       lexer
commaSep1      = T.commaSep1      lexer
