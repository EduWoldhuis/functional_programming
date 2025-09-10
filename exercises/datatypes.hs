-- 1
data Nat = Zero | Succ Nat
  deriving (Show, Eq)

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ i) = 1 + toInt i

fromInt :: Int -> Nat
fromInt i 
  | i <= 0     = Zero
  | otherwise  = Succ (fromInt (i-1))

-- 3
data Complex = C Float Float

toComplex :: Float -> Float -> Complex
toComplex a b = C a b

instance Eq Complex where 
  (C a1 b1) == (C a2 b2) = a1 == a2 && b1 == b2 

instance Show Complex where 
  show (C a1 b1) = ("C" ++ show a1 ++ ", " ++ show b1)

instance Num Complex where
  (C a1 b1) + (C a2 b2) = C (a1+a2) (b1+b2)
  (C a1 b1) - (C a2 b2) = C (a1-a2) (b1-b2)
  (C a1 b1) * (C a2 b2) = C (a1 * a2 - b1 * b2) (a1 * b2 - b1 * a2)
  negate (C a b)        = C (negate a) (negate b)
  abs    (C a b)        = C (a*a+b*b) 0
  fromInteger i         = C (fromInteger i) 0

-- 4  
data Fender = FPlastic | FMetal
  deriving (Show, Eq)

data Gears = G Int | DG (Int, Int)
  deriving (Show, Eq)

data Size = Small | Medium | Large | S Int
  deriving (Show, Eq)

data Bike =  City     Size (Maybe Fender) (Maybe Fender)
           | Road     Size Gears
           | Mountain Size Gears
  deriving (Show, Eq)

getFenders :: Bike -> (Maybe Fender, Maybe Fender)
getFenders (City _ f1 f2) = (f1, f2)
getFenders _ = (Nothing, Nothing)

-- 7

data Point = Point Float Float -- Point x y is the point with coordinates (x, y) in the plane
data Vector = Vector Float Float -- Vector dx dy is the 2d vector in the direction (dx, dy)
data EqLine = EqLine Float Float Float -- EqLine a b c represents the line a * x + b * y + c = 0
data VectLine = VectLine Point Vector -- VectLine p v represents the line through p in the direction v 



