module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor (($>))

import AST
import Lexer
import Data.List (intercalate)
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft), Operator(Infix))

parseModule :: Parser Module
parseModule = do
  declarations <- many parseDeclaration
  eof
  return $ Module declarations

parseDeclaration :: Parser Declaration
parseDeclaration = do
  x <- identifier
  symbol ":"
  visibility <- parseVisibility
  Declaration x visibility <$> parseDeclarationValue

parseVisibility :: Parser Visibility
parseVisibility = do
  string "+" $>  Public
  <|> return Private

parseDeclarationValue :: Parser DeclarationValue
parseDeclarationValue =
  parseVarDef
  <|> parseFuncDef
  <|> parseEnumDecl

parseVarDef :: Parser DeclarationValue
parseVarDef = VarDeclarationValue <$> parseVariableDefinition

parseFuncDef :: Parser DeclarationValue
parseFuncDef = FuncDeclarationValue <$> parseFunctionDefinition

parseEnumDecl :: Parser DeclarationValue
parseEnumDecl = EnumDeclarationValue <$> parseEnumDefinition

parseVariableDefinition :: Parser VariableDefinition
parseVariableDefinition = do
  typ <- parseType
  initializer <- optionMaybe $ symbol "=" *> parseExpression
  return $ VariableDefinition typ initializer

parseFunctionDefinition :: Parser FunctionDefinition
parseFunctionDefinition = do
  reserved "fn"
  symbol "("
  parameters <- parseVariableDefinition `sepBy` symbol ","
  symbol ")"
  typ <- option Void $ do
    symbol "->"
    parseType
  functionBody <- optionMaybe parseFunctionBody
  return $ FunctionDefinition parameters typ functionBody

parseFunctionBody :: Parser FunctionBody
parseFunctionBody = do
  symbol "{"
  x <- many parseStatement
  symbol "}"
  return $ FunctionBody x

parseEnumDefinition :: Parser EnumDefinition
parseEnumDefinition = do
  reserved "enum"
  name <- identifier
  symbol "{"
  values <- parseEnumValue `sepBy` symbol ","
  symbol ";"
  members <- semiSep parseInnerDeclaration
  symbol "}"
  return $ EnumDefinition name values members

parseEnumValue :: Parser EnumValue
parseEnumValue = do
  name <- identifier
  parameters <- option [] $ do
    symbol "(" *> parseVariableDefinition `sepBy` symbol "," <* symbol ")"
  memberAssign <- option [] $ do
    symbol "{" *> semiSep parseEnumMemberAssign <* symbol "}"
  return $ EnumValue name parameters memberAssign

parseEnumMemberAssign :: Parser EnumMemberAssign
parseEnumMemberAssign = do
  name <- identifier
  symbol "="
  EnumMember name <$> parseExpression

parseInnerDeclaration :: Parser InnerDeclaration
parseInnerDeclaration = do
  x <- identifier
  symbol ":"
  InnerDeclaration x <$> parseInnerDeclarationValue

parseInnerDeclarationValue :: Parser InnerDeclarationValue
parseInnerDeclarationValue = parseInnerVarDef

parseInnerVarDef :: Parser InnerDeclarationValue
parseInnerVarDef = InnerVarDeclarationValue <$> parseVariableDefinition

parseStatement :: Parser Statement
parseStatement = choice
  [ try parseReturn
  , try parseInnerDecl
  , parseAssignment
  ] <* symbol ";"

parseInnerDecl :: Parser Statement
parseInnerDecl = InnerDecl <$> parseInnerDeclaration

parseAssignment :: Parser Statement
parseAssignment = do
  variableDefinition <- parseVariableDefinition
  symbol "="
  Assignment variableDefinition <$> parseExpression

parseReturn :: Parser Statement
parseReturn = do
  reserved "return"
  Return <$> optionMaybe parseExpression

parseExpression :: Parser Expression
parseExpression = buildExpressionParser ops parseTerm where
  ops = [
      [binary "*" AssocLeft, binary "/" AssocLeft],
      [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  binary name = Infix (reserved name >> return (BinOp name))
  parseTerm = parens parseExpression <|> parseIdent <|> parseLiteral

parseIdent :: Parser Expression
parseIdent = Variable <$> identifier

parseLiteral :: Parser Expression
parseLiteral = choice
  [ parseNumber
  , parseString
  ]

parseNumber :: Parser Expression
parseNumber = NumberLiteral <$> integer

parseString :: Parser Expression
parseString = StringLiteral <$> stringLiteral

parseType :: Parser Type
parseType = Type <$> parseQualifiedName

parseQualifiedName :: Parser String
parseQualifiedName = do
  x <- identifier
  xs <- many $ symbol "." *> identifier
  return $ intercalate "." (x:xs)
