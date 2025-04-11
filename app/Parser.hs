module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Data.Functor (($>))

import AST.AST
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
  [ try parseReturn <* symbol ";"
  , try parseControl
  , try parseInnerDecl <* symbol ";"
  , try parseAssignment <* symbol ";"
  , parseBlock
  ]

parseInnerDecl :: Parser Statement
parseInnerDecl = InnerDecl <$> parseInnerDeclaration

parseAssignment :: Parser Statement
parseAssignment = do
  varRef <- identifier
  symbol "="
  Assignment varRef <$> parseExpression

parseBlock :: Parser Statement
parseBlock = do
  symbol "{"
  statements <- many parseStatement
  symbol "}"
  return $ BlockBody statements

parseAssignExpr :: Parser Expression
parseAssignExpr = do
  varRef <- identifier
  symbol "="
  AssignExpr varRef <$> parseExpression

parseReturn :: Parser Statement
parseReturn = do
  reserved "return"
  Return <$> optionMaybe parseExpression

parseControl :: Parser Statement
parseControl = choice
  [ try parseIf
  , try parseWhile
  , parseFor
  ]

parseIf :: Parser Statement
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

parseWhile :: Parser Statement
parseWhile = do
  reserved "while"
  symbol "("
  condition <- parseExpression
  symbol ")"
  While condition <$> parseStatement

parseFor :: Parser Statement
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

parseExpression :: Parser Expression
parseExpression = choice
  [ try parseTernary
  , try parseAssignExpr
  , parseBinary
  ]

parseTernary :: Parser Expression
parseTernary = do
  condition <- parseBinary
  symbol "?"
  trueBranch <- parseExpression
  symbol ":"
  Ternary condition trueBranch <$> parseExpression

parseBinary :: Parser Expression
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
