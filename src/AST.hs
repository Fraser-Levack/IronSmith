module AST where

-- 1. Expressions: Anything that calculates a number
data Expr 
    = Lit Float          -- A literal number, e.g., 10
    | Var String         -- A variable, e.g., "x"
    | Add Expr Expr      -- Addition, e.g., x + 2
    | Mul Expr Expr      -- Multiplication, e.g., y * 2
    deriving (Show, Eq)

-- 2. Shapes now accept Expressions instead of raw Floats
data Shape 
    = Cube Expr Expr Expr
    | Cylinder Expr Expr Expr
    | Cone Expr Expr Expr Expr  -- radius, top radius, definition, height
    | Torus Expr Expr Expr  -- radius, tube radius, definition
    | Sphere Expr Expr
    | Move Expr Expr Expr Shape
    | RotateX Expr Shape
    | RotateY Expr Shape
    | RotateZ Expr Shape
    | Group [Shape]
    | ShapeRef String
    deriving (Show, Eq)

-- 3. Statements: A single line of code in IronSmith
data Statement 
    = Assign String Expr -- e.g., x = 10
    | AssignShape String Shape
    | Draw Shape         -- e.g., cube(x, y, z)
    deriving (Show, Eq)

-- A full script is just a list of statements
type Script = [Statement]