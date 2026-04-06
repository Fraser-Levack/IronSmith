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
    space1                         
    (L.skipLineComment "//")       
    empty                          

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

float :: Parser Float
float = lexeme (L.signed (return ()) (try L.float <|> (fromIntegral <$> L.decimal)))

identifier :: Parser String
identifier = lexeme ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

-------------------------------------------------
-- 1. EXPRESSION PARSER
-------------------------------------------------
pTerm :: Parser Expr
pTerm = choice
  [ Lit <$> float <?> "number"
  , try $ do
      name <- identifier
      notFollowedBy (char '(') 
      return (Var name)
  , between (symbol "(") (symbol ")") pExpr <?> "expression in parentheses"
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Mul <$ symbol "*") ] 
  , [ InfixL (Add <$ symbol "+") ] 
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable <?> "mathematical expression"


-------------------------------------------------
-- 2. SHAPE PARSERS
-------------------------------------------------
-- | NEW FIX: This helper guarantees the parser commits to a shape 
-- even if the user puts spaces between the word and the parenthesis!
shapeKeyword :: String -> Parser ()
shapeKeyword name = try $ do
    _ <- symbol name
    _ <- symbol "("
    return ()

pCube :: Parser Shape
pCube = do
    shapeKeyword "cube"
    x <- pExpr <?> "first argument (X size) for cube"
    _ <- symbol "," <?> "comma"
    y <- pExpr <?> "second argument (Y size) for cube"
    _ <- symbol "," <?> "comma"
    z <- pExpr <?> "third argument (Z size) for cube"
    _ <- symbol ")" <?> "closing ')' for cube"
    return (Cube x y z)

pCylinder :: Parser Shape
pCylinder = do
    shapeKeyword "cylinder"
    r <- pExpr <?> "radius argument for cylinder"
    _ <- symbol "," <?> "comma"
    d <- pExpr <?> "definition argument for cylinder"
    _ <- symbol "," <?> "comma"
    h <- pExpr <?> "height argument for cylinder"
    _ <- symbol ")" <?> "closing ')' for cylinder"
    return (Cylinder r d h)

pSphere :: Parser Shape
pSphere = do
    shapeKeyword "sphere"
    r <- pExpr <?> "radius argument for sphere"
    _ <- symbol "," <?> "comma"
    d <- pExpr <?> "definition argument for sphere"
    _ <- symbol ")" <?> "closing ')' for sphere"
    return (Sphere r d)

pCone :: Parser Shape
pCone = do
    shapeKeyword "cone"
    r <- pExpr <?> "base radius for cone"
    _ <- symbol "," <?> "comma"
    -- Handle both 3-arg and 4-arg versions cleanly
    let parse4 = try $ do
            tr <- pExpr <?> "top radius for cone"
            _ <- symbol "," <?> "comma"
            d <- pExpr <?> "definition for cone"
            _ <- symbol "," <?> "comma"
            h <- pExpr <?> "height for cone"
            return (tr, d, h)
        parse3 = do
            d <- pExpr <?> "definition for cone"
            _ <- symbol "," <?> "comma"
            h <- pExpr <?> "height for cone"
            return (Lit 0, d, h)
            
    (tr, d, h) <- parse4 <|> parse3
    _ <- symbol ")" <?> "closing ')' for cone"
    return (Cone r tr d h)

pTorus :: Parser Shape
pTorus = do
    shapeKeyword "torus"
    r <- pExpr <?> "main radius for torus"
    _ <- symbol "," <?> "comma"
    tr <- pExpr <?> "tube radius for torus"
    _ <- symbol "," <?> "comma"
    d <- pExpr <?> "definition for torus"
    _ <- symbol ")" <?> "closing ')' for torus"
    return (Torus r tr d)

pMove :: Parser Shape
pMove = do
    shapeKeyword "move"
    x <- pExpr <?> "X offset for move"
    _ <- symbol "," <?> "comma"
    y <- pExpr <?> "Y offset for move"
    _ <- symbol "," <?> "comma"
    z <- pExpr <?> "Z offset for move"
    _ <- symbol "," <?> "comma"
    innerShape <- pShape <?> "shape to move"
    _ <- symbol ")" <?> "closing ')' for move"
    return (Move x y z innerShape)

-- Helper to quickly generate rotation parsers
pRotate :: String -> (Expr -> Shape -> Shape) -> Parser Shape
pRotate name constructor = do
    shapeKeyword name
    deg <- pExpr <?> ("degrees to " ++ name)
    _ <- symbol "," <?> "comma"
    innerShape <- pShape <?> ("shape to " ++ name)
    _ <- symbol ")" <?> ("closing ')' for " ++ name)
    return (constructor deg innerShape)

pRotateX :: Parser Shape
pRotateX = pRotate "rotateX" RotateX

pRotateY :: Parser Shape
pRotateY = pRotate "rotateY" RotateY

pRotateZ :: Parser Shape
pRotateZ = pRotate "rotateZ" RotateZ

pGroup :: Parser Shape
pGroup = do
    shapeKeyword "group"
    shapes <- pShape `sepBy` symbol "," <?> "comma-separated shapes for group"
    _ <- symbol ")" <?> "closing ')' for group"
    return (Group shapes)

pBinaryCSG :: String -> (Shape -> Shape -> Shape) -> Parser Shape
pBinaryCSG keyword constructor = do
    shapeKeyword keyword
    shape1 <- pShape <?> ("first shape for " ++ keyword)
    _ <- symbol "," <?> "comma"
    shape2 <- pShape <?> ("second shape for " ++ keyword)
    _ <- symbol ")" <?> ("closing ')' for " ++ keyword)
    return (constructor shape1 shape2)

pUnion :: Parser Shape
pUnion = pBinaryCSG "union" Union

pIntersection :: Parser Shape
pIntersection = pBinaryCSG "intersection" Intersection

pDifference :: Parser Shape
pDifference = pBinaryCSG "difference" Difference

pShapeRef :: Parser Shape
pShapeRef = ShapeRef <$> identifier <?> "shape variable name"

pShape :: Parser Shape
pShape = pGroup <|>
         pUnion <|>
         pIntersection <|>
         pDifference <|>
         pRotateX <|> 
         pRotateY <|> 
         pRotateZ <|> 
         pMove <|> 
         pCylinder <|> 
         pSphere <|> 
         pCube <|> 
         pCone <|>
         pTorus <|> 
         pShapeRef

-------------------------------------------------
-- 3. STATEMENT PARSERS 
-------------------------------------------------
pAssign :: Parser Statement
pAssign = try $ do
    name <- identifier
    _ <- symbol "="
    Assign name <$> pExpr <?> "number expression"

pAssignShape :: Parser Statement
pAssignShape = try $ do
    name <- identifier
    _ <- symbol "="
    AssignShape name <$> pShape <?> "shape expression"

pDraw :: Parser Statement
pDraw = Draw <$> pShape

pStatement :: Parser Statement
pStatement = pAssign <|> pAssignShape <|> pDraw <?> "statement (e.g., shape = cube(...) or cube(...))"

-------------------------------------------------
-- 4. SCRIPT PARSER 
-------------------------------------------------
pScript :: Parser Script
pScript = sc *> many pStatement <* eof