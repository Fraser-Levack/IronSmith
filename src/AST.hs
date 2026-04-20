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
    | Cylinder Expr Expr
    | Cone Expr Expr Expr  -- radius, top radius, definition, height
    | Torus Expr Expr  -- radius, tube radius, definition
    | Sphere Expr
    | Move Expr Expr Expr Shape
    | RotateX Expr Shape
    | RotateY Expr Shape
    | RotateZ Expr Shape
    | Group [Shape]
    | Union Shape Shape
    | Intersection Shape Shape
    | Difference Shape Shape
    | ShapeRef String
    | Repeat Expr Expr Expr Shape -- spacing in X, Y, Z, and the shape
    | Paint String [Shape]
    | Material String [Shape]
    | Scale Expr Expr Expr Shape
    deriving (Show, Eq)

-- 3. Statements: A single line of code in IronSmith
data Statement 
    = Assign String Expr -- e.g., x = 10
    | AssignShape String Shape
    | Draw Shape         -- e.g., cube(x, y, z)
    deriving (Show, Eq)

-- A full script is just a list of statements
type Script = [Statement]