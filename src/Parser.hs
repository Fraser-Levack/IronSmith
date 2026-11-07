module Parser where

import AST
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (void)
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
  [ Lit <$> try float <?> "number"
    -- Unary minus on anything that isn't a plain literal: -x, -(a + b).
    -- Literals keep parsing as Lit (-5) via the signed float above.
  , Sub (Lit 0) <$> (symbol "-" *> pTerm)
  , try $ do
      name <- identifier
      notFollowedBy (char '(') 
      return (Var name)
  , between (symbol "(") (symbol ")") pExpr <?> "expression in parentheses"
  ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/") ]
  , [ InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-") ]
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

-- | Separator between a call's arguments. Hitting ')' where another
--   argument was expected means too few arguments, so say what the call
--   should look like instead of a generic "expecting comma".
argSep :: String -> Parser ()
argSep usage = do
    early <- optional (lookAhead (symbol ")"))
    case early of
        Just _  -> fail ("too few arguments; expected " ++ usage)
        Nothing -> void (symbol "," <?> "comma")

-- | The closing ')' of a call. A ',' here means too many arguments
--   (e.g. the old sphere(r, 16) form), so show the correct form.
pClose :: String -> Parser ()
pClose usage = do
    extra <- optional (lookAhead (symbol ","))
    case extra of
        Just _  -> fail ("too many arguments; expected " ++ usage)
        Nothing -> void (symbol ")" <?> ("closing ')' for " ++ usage))

pCube :: Parser Shape
pCube = do
    shapeKeyword "cube"
    let usage = "cube(width, height, depth)"
    x <- pExpr <?> "first argument (X size) for cube"
    argSep usage
    y <- pExpr <?> "second argument (Y size) for cube"
    argSep usage
    z <- pExpr <?> "third argument (Z size) for cube"
    pClose usage
    return (Cube x y z)

pCylinder :: Parser Shape
pCylinder = do
    shapeKeyword "cylinder"
    let usage = "cylinder(radius, height)"
    r <- pExpr <?> "radius argument for cylinder"
    argSep usage
    h <- pExpr <?> "height argument for cylinder"
    pClose usage
    return (Cylinder r h)

pSphere :: Parser Shape
pSphere = do
    shapeKeyword "sphere"
    let usage = "sphere(radius)"
    r <- pExpr <?> "radius argument for sphere"
    pClose usage
    return (Sphere r)

pCone :: Parser Shape
pCone = do
    shapeKeyword "cone"
    let usage = "cone(radius, height) or cone(radius, top_radius, height)"
    r <- pExpr <?> "base radius for cone"
    argSep usage
    
    -- Updated to choose between (top_radius, height) or just (height)
    let parse3 = try $ do
            tr <- pExpr <?> "top radius for cone"
            argSep usage
            h <- pExpr <?> "height for cone"
            return (tr, h)
        parse2 = do
            h <- pExpr <?> "height for cone"
            return (Lit 0, h)
            
    (tr, h) <- parse3 <|> parse2
    pClose usage
    return (Cone r tr h)

pTorus :: Parser Shape
pTorus = do
    shapeKeyword "torus"
    let usage = "torus(radius, tube_radius)"
    r <- pExpr <?> "main radius for torus"
    argSep usage
    tr <- pExpr <?> "tube radius for torus"
    pClose usage
    return (Torus r tr)

pMove :: Parser Shape
pMove = do
    shapeKeyword "move"
    let usage = "move(x, y, z, shape)"
    x <- pExpr <?> "X offset for move"
    argSep usage
    y <- pExpr <?> "Y offset for move"
    argSep usage
    z <- pExpr <?> "Z offset for move"
    argSep usage
    innerShape <- pShape <?> "shape to move"
    pClose usage
    return (Move x y z innerShape)

pRepeat :: Parser Shape
pRepeat = do
    shapeKeyword "repeat"
    let usage = "repeat(x_spacing, y_spacing, z_spacing, shape)"
    x <- pExpr <?> "X spacing"
    argSep usage
    y <- pExpr <?> "Y spacing"
    argSep usage
    z <- pExpr <?> "Z spacing"
    argSep usage
    innerShape <- pShape <?> "shape to repeat"
    pClose usage
    return (Repeat x y z innerShape)

-- Helper to quickly generate rotation parsers
pRotate :: String -> (Expr -> Shape -> Shape) -> Parser Shape
pRotate name constructor = do
    shapeKeyword name
    let usage = name ++ "(degrees, shape)"
    deg <- pExpr <?> ("degrees to " ++ name)
    argSep usage
    innerShape <- pShape <?> ("shape to " ++ name)
    pClose usage
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
    let usage = "scale(factor, shape) or scale(x, y, z, shape)"
    
    -- Try reading 3 arguments first (X, Y, Z)
    let parse3 = try $ do
            sx <- pExpr <?> "X scale factor"
            argSep usage
            sy <- pExpr <?> "Y scale factor"
            argSep usage
            sz <- pExpr <?> "Z scale factor"
            argSep usage
            return (sx, sy, sz)
            
    -- If it fails, fall back to 1 uniform argument
        parse1 = do
            s <- pExpr <?> "uniform scale factor"
            argSep usage
            return (s, s, s)

    -- Choose the successful parser
    (sx, sy, sz) <- parse3 <|> parse1
    
    innerShape <- pShape <?> "shape to scale"
    pClose usage
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
    let usage = keyword ++ "(shape_a, shape_b)"
    shape1 <- pShape <?> ("first shape for " ++ keyword)
    argSep usage
    shape2 <- pShape <?> ("second shape for " ++ keyword)
    pClose usage
    return (constructor shape1 shape2)

pUnion :: Parser Shape
pUnion = pBinaryCSG "union" Union

pIntersection :: Parser Shape
pIntersection = pBinaryCSG "intersection" Intersection

pDifference :: Parser Shape
pDifference = pBinaryCSG "difference" Difference

-- | A colour or material name, optionally wrapped in double quotes, so
--   both paint(#FF5733, ...) and paint("#FF5733", ...) work.
optQuoted :: Parser String -> Parser String
optQuoted p = lexeme (between (char '"') (char '"') p <|> p)

pHexColor :: Parser String
pHexColor = (:) <$> char '#' <*> count 6 hexDigitChar

pPresetColor :: Parser String
pPresetColor = choice (map string ["white", "black", "red", "blue", "green"])

pColor :: Parser String
pColor = optQuoted (pHexColor <|> pPresetColor)

pPaint :: Parser Shape
pPaint = do
    shapeKeyword "paint"
    col <- pColor <?> "color (hex or preset)"
    _ <- symbol "," <?> "comma"
    shapes <- pShape `sepBy` symbol "," <?> "comma-separated shapes to paint"
    _ <- symbol ")" <?> "closing ')' for paint"
    return (Paint col shapes)

pMaterialPreset :: Parser String
pMaterialPreset = optQuoted (choice (map string ["matte", "plastic", "neon", "metal"]))

pMaterial :: Parser Shape
pMaterial = do
    shapeKeyword "material"
    mat <- pMaterialPreset <?> "material preset (matte, plastic, neon, metal)"
    _ <- symbol "," <?> "comma"
    shapes <- pShape `sepBy` symbol "," <?> "comma-separated shapes for material"
    _ <- symbol ")" <?> "closing ')' for material"
    return (Material mat shapes)

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
         pMaterial <|>
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

-- | light(x, y, z) places a directional light shining from that position
--   toward the origin. An optional 4th argument sets its intensity
--   (default 1.0). The last light statement in a script wins.
pLight :: Parser Statement
pLight = do
    shapeKeyword "light"
    let usage = "light(x, y, z) or light(x, y, z, intensity)"
    x <- pExpr <?> "light X position"
    argSep usage
    y <- pExpr <?> "light Y position"
    argSep usage
    z <- pExpr <?> "light Z position"
    i <- option (Lit 1.0) (symbol "," *> (pExpr <?> "light intensity"))
    pClose usage
    return (Light x y z i)

-- | camera(t, yaw, pitch, dist) places a camera keyframe at t seconds on
--   the animation timeline. Yaw/pitch are in degrees. An optional trailing
--   x, y, z sets the point the camera orbits/looks at (default: origin).
--   Two or more keyframes define a looping camera animation.
pCameraKey :: Parser Statement
pCameraKey = do
    shapeKeyword "camera"
    let usage = "camera(t, yaw, pitch, dist) or camera(t, yaw, pitch, dist, x, y, z)"
    t <- pExpr <?> "keyframe time (seconds) for camera"
    argSep usage
    yaw <- pExpr <?> "camera yaw (degrees)"
    argSep usage
    pitch <- pExpr <?> "camera pitch (degrees)"
    argSep usage
    dist <- pExpr <?> "camera distance"
    (tx, ty, tz) <- option (Lit 0, Lit 0, Lit 0) $ do
        _ <- symbol ","
        x <- pExpr <?> "camera target X"
        argSep usage
        y <- pExpr <?> "camera target Y"
        argSep usage
        z <- pExpr <?> "camera target Z"
        return (x, y, z)
    pClose usage
    return (CameraKey t yaw pitch dist tx ty tz)

pStatement :: Parser Statement
pStatement = pLight <|> pCameraKey <|> pAssign <|> pAssignShape <|> pDraw <?> "statement (e.g., shape = cube(...) or cube(...))"

-------------------------------------------------
-- 4. SCRIPT PARSER 
-------------------------------------------------
pScript :: Parser Script
pScript = sc *> many pStatement <* eof