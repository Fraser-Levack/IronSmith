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
    h <- pExpr <?> "height argument for cylinder"
    _ <- symbol ")" <?> "closing ')' for cylinder"
    return (Cylinder r h)

pSphere :: Parser Shape
pSphere = do
    shapeKeyword "sphere"
    r <- pExpr <?> "radius argument for sphere"
    _ <- symbol ")" <?> "closing ')' for sphere"
    return (Sphere r)

pCone :: Parser Shape
pCone = do
    shapeKeyword "cone"
    r <- pExpr <?> "base radius for cone"
    _ <- symbol "," <?> "comma"
    
    -- Updated to choose between (top_radius, height) or just (height)
    let parse3 = try $ do
            tr <- pExpr <?> "top radius for cone"
            _ <- symbol "," <?> "comma"
            h <- pExpr <?> "height for cone"
            return (tr, h)
        parse2 = do
            h <- pExpr <?> "height for cone"
            return (Lit 0, h)
            
    (tr, h) <- parse3 <|> parse2
    _ <- symbol ")" <?> "closing ')' for cone"
    return (Cone r tr h)

pTorus :: Parser Shape
pTorus = do
    shapeKeyword "torus"
    r <- pExpr <?> "main radius for torus"
    _ <- symbol "," <?> "comma"
    tr <- pExpr <?> "tube radius for torus"
    _ <- symbol ")" <?> "closing ')' for torus"
    return (Torus r tr)

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

pRepeat :: Parser Shape
pRepeat = do
    shapeKeyword "repeat"
    x <- pExpr <?> "X spacing"
    _ <- symbol "," <?> "comma"
    y <- pExpr <?> "Y spacing"
    _ <- symbol "," <?> "comma"
    z <- pExpr <?> "Z spacing"
    _ <- symbol "," <?> "comma"
    innerShape <- pShape <?> "shape to repeat"
    _ <- symbol ")" <?> "closing ')' for repeat"
    return (Repeat x y z innerShape)

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

pScale :: Parser Shape
pScale = do
    shapeKeyword "scale"
    
    -- Try reading 3 arguments first (X, Y, Z)
    let parse3 = try $ do
            sx <- pExpr <?> "X scale factor"
            _ <- symbol "," <?> "comma"
            sy <- pExpr <?> "Y scale factor"
            _ <- symbol "," <?> "comma"
            sz <- pExpr <?> "Z scale factor"
            _ <- symbol "," <?> "comma"
            return (sx, sy, sz)
            
    -- If it fails, fall back to 1 uniform argument
        parse1 = do
            s <- pExpr <?> "uniform scale factor"
            _ <- symbol "," <?> "comma"
            return (s, s, s)

    -- Choose the successful parser
    (sx, sy, sz) <- parse3 <|> parse1
    
    innerShape <- pShape <?> "shape to scale"
    _ <- symbol ")" <?> "closing ')' for scale"
    return (Scale sx sy sz innerShape)

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

pHexColor :: Parser String
pHexColor = lexeme $ do
    _ <- char '#'
    digits <- count 6 hexDigitChar
    return ('#' : digits)

pPresetColor :: Parser String
pPresetColor = lexeme (choice (map symbol ["white", "black", "red", "blue", "green"]))

pColor :: Parser String
pColor = try pHexColor <|> pPresetColor

pPaint :: Parser Shape
pPaint = do
    shapeKeyword "paint"
    col <- pColor <?> "color (hex or preset)"
    _ <- symbol "," <?> "comma"
    shapes <- pShape `sepBy` symbol "," <?> "comma-separated shapes to paint"
    _ <- symbol ")" <?> "closing ')' for paint"
    return (Paint col shapes)

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
         pRepeat <|>
         pPaint <|>
         pScale <|>
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