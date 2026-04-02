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
float = lexeme (L.signed (return ()) (try L.float <|> (fromIntegral <$> L.decimal)))

-- Parses variable names (e.g., "x", "myVar")
identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-------------------------------------------------
-- 1. EXPRESSION PARSER (The Math)
-------------------------------------------------
pTerm :: Parser Expr
pTerm = choice
  [ Lit <$> float
  , try $ do
      name <- identifier
      notFollowedBy (char '(') -- NEW: Reject if a parenthesis comes right after!
      return (Var name)
  , between (symbol "(") (symbol ")") pExpr 
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

pCylinder :: Parser Shape
pCylinder = do
    _ <- symbol "cylinder("
    r <- pExpr; _ <- symbol ","
    d <- pExpr; _ <- symbol ","
    h <- pExpr; _ <- symbol ")"
    return (Cylinder r d h)

pSphere :: Parser Shape
pSphere = do
    _ <- symbol "sphere("
    r <- pExpr; _ <- symbol ","
    d <- pExpr; _ <- symbol ")"
    return (Sphere r d)

-- Parses the explicit 4-argument version: cone(r, tr, d, h)
pCone4 :: Parser Shape
pCone4 = do
    _ <- symbol "cone("
    r <- pExpr; _ <- symbol ","
    tr <- pExpr; _ <- symbol ","
    d <- pExpr; _ <- symbol ","
    h <- pExpr; _ <- symbol ")"
    return (Cone r tr d h)

-- Parses the shorthand 3-argument version: cone(r, d, h)
pCone3 :: Parser Shape
pCone3 = do
    _ <- symbol "cone("
    r <- pExpr; _ <- symbol ","
    -- Notice we don't parse 'tr' here!
    d <- pExpr; _ <- symbol ","
    h <- pExpr; _ <- symbol ")"
    -- We secretly inject (Lit 0) as the top radius
    return (Cone r (Lit 0) d h)

pTorus :: Parser Shape
pTorus = do
    _ <- symbol "torus("
    r <- pExpr; _ <- symbol ","
    tr <- pExpr; _ <- symbol ","
    d <- pExpr; _ <- symbol ")"
    return (Torus r tr d)

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

pRotateY :: Parser Shape
pRotateY = do
    _ <- symbol "rotateY("
    deg <- pExpr
    _ <- symbol ","
    innerShape <- pShape
    _ <- symbol ")"
    return (RotateY deg innerShape)

pRotateZ :: Parser Shape
pRotateZ = do
    _ <- symbol "rotateZ("
    deg <- pExpr
    _ <- symbol ","
    innerShape <- pShape
    _ <- symbol ")"
    return (RotateZ deg innerShape)

pGroup :: Parser Shape
pGroup = do
    _ <- symbol "group("
    -- sepBy automatically handles reading items separated by commas!
    shapes <- pShape `sepBy` symbol ","
    _ <- symbol ")"
    return (Group shapes)


pShapeRef :: Parser Shape
pShapeRef = ShapeRef <$> identifier

pShape :: Parser Shape
pShape = try pGroup <|> 
         try pRotateX <|> 
         try pRotateY <|> 
         try pRotateZ <|> 
         try pMove <|> 
         try pCylinder <|> 
         try pSphere <|> 
         try pCube <|> 
         try pCone4 <|> 
         try pCone3 <|>
         try pTorus <|> 
         pShapeRef
-------------------------------------------------
-- 3. STATEMENT PARSERS (The Actions)
-------------------------------------------------
pAssign :: Parser Statement
pAssign = do
    name <- identifier
    _ <- symbol "="
    Assign name <$> pExpr

pAssignShape :: Parser Statement
pAssignShape = do
    name <- identifier
    _ <- symbol "="
    AssignShape name <$> pShape

pDraw :: Parser Statement
pDraw = Draw <$> pShape

pStatement :: Parser Statement
pStatement = try pAssign <|> try pAssignShape <|> pDraw

-------------------------------------------------
-- 3. SCRIPT PARSER (The Whole File)
-------------------------------------------------
-- Reads whitespace, then as many statements as possible until End Of File (eof)
pScript :: Parser Script
pScript = sc *> many pStatement <* eof