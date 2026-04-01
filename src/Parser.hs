-- src/Parser.hs
module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

-- Helpers to ignore whitespace
sc :: Parser ()
sc = L.space 
    space1                         -- Consume normal whitespace
    (L.skipLineComment "//")       -- Consume line comments starting with "//"
    empty                          -- No block comments (yet)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Parses numbers
float :: Parser Float
float = lexeme (try L.float <|> (fromIntegral <$> L.decimal))

-- Parses variable names (e.g., "x", "myVar")
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many alphaNumChar)

-------------------------------------------------
-- 1. EXPRESSION PARSER (The Math)
-------------------------------------------------
pTerm :: Parser Expr
pTerm = choice
  [ Lit <$> float
  , Var <$> identifier
  , between (symbol "(") (symbol ")") pExpr -- Allows (x + 2)
  ]

-- This table defines our math operators and their precedence
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Mul <$ symbol "*") ] -- Multiplication comes first
  , [ InfixL (Add <$ symbol "+") ] -- Then Addition
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable


-------------------------------------------------
-- 2. SHAPE PARSERS
-------------------------------------------------
pCube :: Parser Shape
pCube = do
    _ <- symbol "cube("
    x <- pExpr
    _ <- symbol ","
    y <- pExpr
    _ <- symbol ","
    z <- pExpr
    _ <- symbol ")"
    return (Cube x y z)

pMove :: Parser Shape
pMove = do
    _ <- symbol "move("
    x <- pExpr
    _ <- symbol ","
    y <- pExpr
    _ <- symbol ","
    z <- pExpr
    _ <- symbol ","
    innerShape <- pShape -- Recursively read the shape inside!
    _ <- symbol ")"
    return (Move x y z innerShape)

pRotateX :: Parser Shape
pRotateX = do
    _ <- symbol "rotateX("
    deg <- pExpr
    _ <- symbol ","
    innerShape <- pShape
    _ <- symbol ")"
    return (RotateX deg innerShape)

-- UPDATE pShape to include try pRotateX
pShape :: Parser Shape
pShape = try pRotateX <|> try pMove <|> pCube
-------------------------------------------------
-- 3. STATEMENT PARSERS (The Actions)
-------------------------------------------------
pAssign :: Parser Statement
pAssign = do
    name <- identifier
    _ <- symbol "="
    Assign name <$> pExpr

pDraw :: Parser Statement
pDraw = Draw <$> pShape  -- Draw whatever shape pShape finds

pStatement :: Parser Statement
pStatement = try pAssign <|> pDraw

-------------------------------------------------
-- 3. SCRIPT PARSER (The Whole File)
-------------------------------------------------
-- Reads whitespace, then as many statements as possible until End Of File (eof)
pScript :: Parser Script
pScript = sc *> many pStatement <* eof