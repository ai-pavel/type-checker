{-# LANGUAGE OverloadedStrings #-}

module Infer.Parser
  ( parseExpr
  , parsePattern
  ) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Infer.Ast

type Parser = Parsec Void String

-- | Whitespace consumer (including line comments)
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parse between parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Reserved words
reservedWords :: [String]
reservedWords = ["let", "in", "if", "then", "else", "case", "of", "True", "False"]

-- | Parse an identifier (not a reserved word)
identifier :: Parser String
identifier = lexeme $ try $ do
  x <- (:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_' <|> char '\'')
  if x `elem` reservedWords
    then fail $ "keyword " ++ show x ++ " cannot be an identifier"
    else return x

-- | Parse a constructor name (starts with uppercase)
constructor :: Parser String
constructor = lexeme $ try $ do
  x <- (:) <$> upperChar <*> many (alphaNumChar <|> char '_' <|> char '\'')
  if x `elem` ["True", "False"]
    then fail $ show x ++ " is a built-in literal"
    else return x

-- | Parse a reserved word
reserved :: String -> Parser ()
reserved w = lexeme $ try $ do
  _ <- string w
  notFollowedBy (alphaNumChar <|> char '_' <|> char '\'')

-- | Parse an integer literal
intLit :: Parser Expr
intLit = ELit . LInt <$> lexeme L.decimal

-- | Parse a boolean literal
boolLit :: Parser Expr
boolLit = ELit (LBool True)  <$ reserved "True"
      <|> ELit (LBool False) <$ reserved "False"

-- | Parse a string literal
stringLit :: Parser Expr
stringLit = ELit . LString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parse an atom (smallest expression unit)
atom :: Parser Expr
atom = choice
  [ parens exprParser
  , boolLit
  , stringLit
  , try intLit
  , try (ECon <$> constructor)
  , EVar <$> identifier
  ]

-- | Parse function application (left-associative juxtaposition)
appExpr :: Parser Expr
appExpr = do
  atoms <- some atom
  return $ foldl1 EApp atoms

-- | Parse a lambda expression
lamExpr :: Parser Expr
lamExpr = do
  _ <- symbol "\\"
  args <- some identifier
  _ <- symbol "->"
  body <- exprParser
  return $ foldr ELam body args

-- | Parse a let expression
letExpr :: Parser Expr
letExpr = do
  reserved "let"
  name <- identifier
  _ <- symbol "="
  val <- exprParser
  reserved "in"
  body <- exprParser
  return $ ELet name val body

-- | Parse an if-then-else expression
ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- exprParser
  reserved "then"
  thenE <- exprParser
  reserved "else"
  elseE <- exprParser
  return $ EIf cond thenE elseE

-- | Parse a case expression
caseExpr :: Parser Expr
caseExpr = do
  reserved "case"
  scrut <- exprParser
  reserved "of"
  branches <- some parseBranch
  return $ ECase scrut branches
  where
    parseBranch :: Parser CaseBranch
    parseBranch = do
      _ <- symbol "|"
      pat <- parsePattern
      _ <- symbol "->"
      body <- exprParser
      return (pat, body)

-- | Parse a pattern
parsePattern :: Parser Pattern
parsePattern = choice
  [ PWild <$ symbol "_"
  , PLit (LBool True)  <$ reserved "True"
  , PLit (LBool False) <$ reserved "False"
  , PLit . LInt <$> lexeme L.decimal
  , PLit . LString <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))
  , try conPattern
  , PVar <$> identifier
  ]
  where
    conPattern :: Parser Pattern
    conPattern = do
      name <- constructor
      args <- many atomPattern
      return $ PCon name args

    atomPattern :: Parser Pattern
    atomPattern = choice
      [ parens parsePattern
      , PWild <$ symbol "_"
      , PVar <$> identifier
      ]

-- | Top-level expression parser
exprParser :: Parser Expr
exprParser = choice
  [ lamExpr
  , letExpr
  , ifExpr
  , caseExpr
  , appExpr
  ]

-- | Parse a string into an expression
parseExpr :: String -> Either String Expr
parseExpr input =
  case parse (sc *> exprParser <* eof) "<input>" input of
    Left err  -> Left (errorBundlePretty err)
    Right ast -> Right ast
