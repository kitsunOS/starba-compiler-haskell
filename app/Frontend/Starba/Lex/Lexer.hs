module Frontend.Starba.Lex.Lexer where

import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token

languageDef :: LanguageDef ()
languageDef = emptyDef {
  Token.reservedNames = ["fn"],
  Token.commentLine = "//",
  Token.commentStart = "/*",
  Token.commentEnd = "*/"
}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

reserved :: String -> Parser ()
reserved = Token.reserved lexer

semiSep :: Parser a -> Parser [a]
semiSep = Token.semiSep lexer

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer
