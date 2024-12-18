module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor (($>))

import AST
import Lexer

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
  parseVarDecl
  <|> parseFuncDecl
  <|> parseVarDef
  <|> parseFuncDef
  <|> parseEnumDecl

parseVarDef :: Parser DeclarationValue
parseVarDef = VarDef <$> parseVariableDefinition

parseVarDecl :: Parser DeclarationValue
parseVarDecl = do
  variableDefinition <- parseVariableDefinition
  symbol "="
  VarDecl variableDefinition <$> parseExpression

parseFuncDef :: Parser DeclarationValue
parseFuncDef = FuncDef <$> parseFunctionDefinition

parseFuncDecl :: Parser DeclarationValue
parseFuncDecl = do
  functionDefinition <- parseFunctionDefinition
  FuncDecl functionDefinition <$> parseFunctionBody

parseEnumDecl :: Parser DeclarationValue
parseEnumDecl = EnumDecl <$> parseEnumDefinition

parseVariableDefinition :: Parser VariableDefinition
parseVariableDefinition = VariableDefinition <$> parseType

parseFunctionDefinition :: Parser FunctionDefinition
parseFunctionDefinition = do
  reserved "fn"
  symbol "("
  parameters <- parseVariableDefinition `sepBy` symbol ","
  symbol ")"
  typ <- option Void $ do
    symbol "->"
    parseType
  return $ FunctionDefinition parameters typ

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
parseInnerDeclarationValue =
  parseInnerVarDecl
  <|> parseInnerVarDef

parseInnerVarDef :: Parser InnerDeclarationValue
parseInnerVarDef = InnerVarDef <$> parseVariableDefinition

parseInnerVarDecl :: Parser InnerDeclarationValue
parseInnerVarDecl = do
  variableDefinition <- parseVariableDefinition
  symbol "="
  InnerVarDecl variableDefinition <$> parseExpression

parseStatement :: Parser Statement
parseStatement = choice
  [ parseInnerDecl
  , parseAssignment
  ] <* symbol ";"

parseInnerDecl :: Parser Statement
parseInnerDecl = InnerDecl <$> parseInnerDeclaration

parseAssignment :: Parser Statement
parseAssignment = do
  variableDefinition <- parseVariableDefinition
  symbol "="
  Assignment variableDefinition <$> parseExpression

parseExpression :: Parser Expression
parseExpression =
    parseNumber
    <|> parseString

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
  return $ unwords $ x:xs
