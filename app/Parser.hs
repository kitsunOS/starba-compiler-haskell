{-# LANGUAGE ScopedTypeVariables #-}

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor (($>))

import AST.AST
import Lexer
import Data.List (intercalate)
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft), Operator(Infix))

type Sym = String

parseModule :: Parser (Module Sym)
parseModule = do
  declarations <- many parseDeclaration
  eof
  return $ Module declarations

parseDeclaration :: Parser (Declaration Sym)
parseDeclaration = do
  x <- identifier
  symbol ":"
  visibility <- parseVisibility
  Declaration x visibility <$> parseDeclarationValue

parseVisibility :: Parser Visibility
parseVisibility = do
  string "+" $>  Public
  <|> return Private

parseDeclarationValue :: Parser (DeclarationValue Sym)
parseDeclarationValue =
  parseVarDef
  <|> parseFuncDef
  <|> parseEnumDecl

parseVarDef :: Parser (DeclarationValue Sym)
parseVarDef = VarDeclarationValue <$> parseVariableDefinition

parseFuncDef :: Parser (DeclarationValue Sym)
parseFuncDef = FuncDeclarationValue <$> parseFunctionDefinition

parseEnumDecl :: Parser (DeclarationValue Sym)
parseEnumDecl = EnumDeclarationValue <$> parseEnumDefinition

parseVariableDefinition :: Parser (VariableDefinition Sym)
parseVariableDefinition = do
  typ <- parseType
  initializer <- optionMaybe $ symbol "=" *> parseExpression
  return $ VariableDefinition typ initializer

parseParameter :: Parser (Parameter Sym)
parseParameter = do
  name <- identifier
  symbol ":"
  Parameter name <$> parseType

parseFunctionDefinition :: Parser (FunctionDefinition Sym)
parseFunctionDefinition = do
  reserved "fn"
  symbol "("
  parameters <- parseParameter `sepBy` symbol ","
  symbol ")"
  typ <- option Void $ do
    symbol "->"
    parseType
  functionBody <- optionMaybe parseFunctionBody
  return $ FunctionDefinition parameters typ functionBody

parseFunctionBody :: Parser (FunctionBody Sym)
parseFunctionBody = do
  symbol "{"
  x <- many parseStatement
  symbol "}"
  return $ FunctionBody x

parseEnumDefinition :: Parser (EnumDefinition Sym)
parseEnumDefinition = do
  reserved "enum"
  name <- identifier
  symbol "{"
  values <- parseEnumValue `sepBy` symbol ","
  symbol ";"
  members <- semiSep parseInnerDeclaration
  symbol "}"
  return $ EnumDefinition name values members

parseEnumValue :: Parser (EnumValue Sym)
parseEnumValue = do
  name <- identifier
  parameters <- option [] $ do
    symbol "(" *> parseParameter `sepBy` symbol "," <* symbol ")"
  memberAssign <- option [] $ do
    symbol "{" *> semiSep parseEnumMemberAssign <* symbol "}"
  return $ EnumValue name parameters memberAssign

parseEnumMemberAssign :: Parser (EnumMemberAssign Sym)
parseEnumMemberAssign = do
  name <- identifier
  symbol "="
  EnumMember name <$> parseExpression

parseInnerDeclaration :: Parser (InnerDeclaration Sym)
parseInnerDeclaration = do
  x <- identifier
  symbol ":"
  InnerDeclaration x <$> parseInnerDeclarationValue

parseInnerDeclarationValue :: Parser (InnerDeclarationValue Sym)
parseInnerDeclarationValue = parseInnerVarDef

parseInnerVarDef :: Parser (InnerDeclarationValue Sym)
parseInnerVarDef = InnerVarDeclarationValue <$> parseVariableDefinition

parseStatement :: Parser (Statement Sym)
parseStatement = choice
  [ try parseReturn <* symbol ";"
  , try parseControl
  , try parseInnerDecl <* symbol ";"
  , try parseAssignment <* symbol ";"
  , parseBlock
  ]

parseInnerDecl :: Parser (Statement Sym)
parseInnerDecl = InnerDecl <$> parseInnerDeclaration

parseAssignment :: Parser (Statement Sym)
parseAssignment = do
  varRef <- identifier
  symbol "="
  Assignment varRef <$> parseExpression

parseBlock :: Parser (Statement Sym)
parseBlock = do
  symbol "{"
  statements <- many parseStatement
  symbol "}"
  return $ BlockBody statements

parseAssignExpr :: Parser (Expression Sym)
parseAssignExpr = do
  varRef <- identifier
  symbol "="
  AssignExpr varRef <$> parseExpression

parseReturn :: Parser (Statement Sym)
parseReturn = do
  reserved "return"
  Return <$> optionMaybe parseExpression

parseControl :: Parser (Statement Sym)
parseControl = choice
  [ try parseIf
  , try parseWhile
  , parseFor
  ]

parseIf :: Parser (Statement Sym)
parseIf = do
  reserved "if"
  symbol "("
  condition <- parseExpression
  symbol ")"
  thenBlock <- parseStatement
  elseBlock <- optionMaybe $ do
    reserved "else"
    parseStatement
  return $ If condition thenBlock elseBlock

parseWhile :: Parser (Statement Sym)
parseWhile = do
  reserved "while"
  symbol "("
  condition <- parseExpression
  symbol ")"
  While condition <$> parseStatement

parseFor :: Parser (Statement Sym)
parseFor = do
  reserved "for"
  symbol "("
  initDecl <- optionMaybe parseInnerDeclaration
  symbol ";"
  condition <- optionMaybe parseExpression
  symbol ";"
  increment <- optionMaybe parseExpression
  symbol ")"
  For initDecl condition increment <$> parseStatement

parseExpression :: Parser (Expression Sym)
parseExpression = choice
  [ try parseTernary
  , try parseAssignExpr
  , parseBinary
  ]

parseTernary :: Parser (Expression Sym)
parseTernary = do
  condition <- parseBinary
  symbol "?"
  trueBranch <- parseExpression
  symbol ":"
  Ternary condition trueBranch <$> parseExpression

parseBinary :: Parser (Expression Sym)
parseBinary = buildExpressionParser ops parseTerm where
  ops = [
      [binary "==" AssocLeft, binary "!=" AssocLeft,
        binary "<" AssocLeft, binary "<=" AssocLeft,
        binary ">" AssocLeft, binary ">=" AssocLeft],
      [binary "*" AssocLeft, binary "/" AssocLeft],
      [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  binary name = Infix (reserved name >> return (BinOp name))
  parseTerm = parens parseExpression <|> parseIdent <|> parseLiteral

parseIdent :: Parser (Expression Sym)
parseIdent = Variable <$> identifier

parseLiteral :: Parser (Expression Sym)
parseLiteral = choice
  [ parseNumber
  , parseString
  ]

parseNumber :: Parser (Expression Sym)
parseNumber = NumberLiteral <$> integer

parseString :: Parser (Expression Sym)
parseString = StringLiteral <$> stringLiteral

parseType :: Parser (Type Sym)
parseType = Type <$> parseQualifiedName

parseQualifiedName :: Parser String
parseQualifiedName = do
  x <- identifier
  xs <- many $ symbol "." *> identifier
  return $ intercalate "." (x:xs)
