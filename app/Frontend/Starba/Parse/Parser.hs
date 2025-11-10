{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Starba.Parse.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr (buildExpressionParser, Assoc (AssocLeft), Operator(Infix))

import Data.Functor (($>))
import Data.List (intercalate)

import qualified Frontend.Starba.AST.AST as AST
import Frontend.Starba.Lex.Lexer

type Sym = String

parseModule :: Parser (AST.Module Sym)
parseModule = do
  declarations <- many parseDeclaration
  eof
  return $ AST.Module declarations

parseDeclaration :: Parser (AST.Declaration Sym)
parseDeclaration = do
  x <- identifier
  symbol ":"
  visibility <- parseVisibility
  AST.Declaration x visibility <$> parseDeclarationValue

parseVisibility :: Parser AST.Visibility
parseVisibility = do
  string "+" $>  AST.Public
  <|> return AST.Private

parseDeclarationValue :: Parser (AST.DeclarationValue Sym)
parseDeclarationValue =
  parseVarDef
  <|> parseFuncDef
  <|> parseEnumDecl

parseVarDef :: Parser (AST.DeclarationValue Sym)
parseVarDef = AST.VarDeclarationValue <$> parseVariableDefinition

parseFuncDef :: Parser (AST.DeclarationValue Sym)
parseFuncDef = AST.FuncDeclarationValue <$> parseFunctionDefinition

parseEnumDecl :: Parser (AST.DeclarationValue Sym)
parseEnumDecl = AST.EnumDeclarationValue <$> parseEnumDefinition

parseVariableDefinition :: Parser (AST.VariableDefinition Sym)
parseVariableDefinition = do
  typ <- parseType
  initializer <- optionMaybe $ symbol "=" *> parseExpression
  return $ AST.VariableDefinition typ initializer

parseParameter :: Parser (AST.Parameter Sym)
parseParameter = do
  name <- identifier
  symbol ":"
  AST.Parameter name <$> parseType

parseFunctionDefinition :: Parser (AST.FunctionDefinition Sym)
parseFunctionDefinition = do
  reserved "fn"
  symbol "("
  parameters <- parseParameter `sepBy` symbol ","
  symbol ")"
  typ <- option AST.Void $ do
    symbol "->"
    parseType
  functionBody <- optionMaybe parseFunctionBody
  return $ AST.FunctionDefinition parameters typ functionBody

parseFunctionBody :: Parser (AST.FunctionBody Sym)
parseFunctionBody = do
  symbol "{"
  x <- many parseStatement
  symbol "}"
  return $ AST.FunctionBody x

parseEnumDefinition :: Parser (AST.EnumDefinition Sym)
parseEnumDefinition = do
  reserved "enum"
  name <- identifier
  symbol "{"
  values <- parseEnumValue `sepBy` symbol ","
  symbol ";"
  members <- semiSep parseInnerDeclaration
  symbol "}"
  return $ AST.EnumDefinition name values members

parseEnumValue :: Parser (AST.EnumValue Sym)
parseEnumValue = do
  name <- identifier
  parameters <- option [] $ do
    symbol "(" *> parseParameter `sepBy` symbol "," <* symbol ")"
  memberAssign <- option [] $ do
    symbol "{" *> semiSep parseEnumMemberAssign <* symbol "}"
  return $ AST.EnumValue name parameters memberAssign

parseEnumMemberAssign :: Parser (AST.EnumMemberAssign Sym)
parseEnumMemberAssign = do
  name <- identifier
  symbol "="
  AST.EnumMember name <$> parseExpression

parseInnerDeclaration :: Parser (AST.InnerDeclaration Sym)
parseInnerDeclaration = do
  x <- identifier
  symbol ":"
  AST.InnerDeclaration x <$> parseInnerDeclarationValue

parseInnerDeclarationValue :: Parser (AST.InnerDeclarationValue Sym)
parseInnerDeclarationValue = parseInnerVarDef

parseInnerVarDef :: Parser (AST.InnerDeclarationValue Sym)
parseInnerVarDef = AST.InnerVarDeclarationValue <$> parseVariableDefinition

parseStatement :: Parser (AST.Statement Sym)
parseStatement = choice
  [ try parseReturn <* symbol ";"
  , try parseControl
  , try parseInnerDecl <* symbol ";"
  , try parseAssignment <* symbol ";"
  , parseBlock
  ]

parseInnerDecl :: Parser (AST.Statement Sym)
parseInnerDecl = AST.InnerDecl <$> parseInnerDeclaration

parseAssignment :: Parser (AST.Statement Sym)
parseAssignment = do
  varRef <- identifier
  symbol "="
  AST.Assignment varRef <$> parseExpression

parseBlock :: Parser (AST.Statement Sym)
parseBlock = do
  symbol "{"
  statements <- many parseStatement
  symbol "}"
  return $ AST.BlockBody statements

parseAssignExpr :: Parser (AST.Expression Sym)
parseAssignExpr = do
  varRef <- identifier
  symbol "="
  AST.AssignExpr varRef <$> parseExpression

parseReturn :: Parser (AST.Statement Sym)
parseReturn = do
  reserved "return"
  AST.Return <$> optionMaybe parseExpression

parseControl :: Parser (AST.Statement Sym)
parseControl = choice
  [ try parseIf
  , try parseWhile
  , parseFor
  ]

parseIf :: Parser (AST.Statement Sym)
parseIf = do
  reserved "if"
  symbol "("
  condition <- parseExpression
  symbol ")"
  thenBlock <- parseStatement
  elseBlock <- optionMaybe $ do
    reserved "else"
    parseStatement
  return $ AST.If condition thenBlock elseBlock

parseWhile :: Parser (AST.Statement Sym)
parseWhile = do
  reserved "while"
  symbol "("
  condition <- parseExpression
  symbol ")"
  AST.While condition <$> parseStatement

parseFor :: Parser (AST.Statement Sym)
parseFor = do
  reserved "for"
  symbol "("
  initDecl <- optionMaybe parseInnerDeclaration
  symbol ";"
  condition <- optionMaybe parseExpression
  symbol ";"
  increment <- optionMaybe parseExpression
  symbol ")"
  AST.For initDecl condition increment <$> parseStatement

parseExpression :: Parser (AST.Expression Sym)
parseExpression = choice
  [ try parseTernary
  , try parseAssignExpr
  , parseBinary
  ]

parseTernary :: Parser (AST.Expression Sym)
parseTernary = do
  condition <- parseBinary
  symbol "?"
  trueBranch <- parseExpression
  symbol ":"
  AST.Ternary condition trueBranch <$> parseExpression

parseBinary :: Parser (AST.Expression Sym)
parseBinary = buildExpressionParser ops parseTerm where
  ops = [
      [binary "==" AssocLeft, binary "!=" AssocLeft,
        binary "<" AssocLeft, binary "<=" AssocLeft,
        binary ">" AssocLeft, binary ">=" AssocLeft],
      [binary "*" AssocLeft, binary "/" AssocLeft],
      [binary "+" AssocLeft, binary "-" AssocLeft]
    ]
  binary name = Infix (reserved name >> return (AST.BinOp name))
  parseTerm = parens parseExpression <|> parseIdent <|> parseLiteral

parseIdent :: Parser (AST.Expression Sym)
parseIdent = AST.Variable <$> identifier

parseLiteral :: Parser (AST.Expression Sym)
parseLiteral = choice
  [ parseNumber
  , parseString
  ]

parseNumber :: Parser (AST.Expression Sym)
parseNumber = AST.NumberLiteral <$> integer

parseString :: Parser (AST.Expression Sym)
parseString = AST.StringLiteral <$> stringLiteral

parseType :: Parser (AST.Type Sym)
parseType = AST.Type <$> parseQualifiedName

parseQualifiedName :: Parser String
parseQualifiedName = do
  x <- identifier
  xs <- many $ symbol "." *> identifier
  return $ intercalate "." (x:xs)
