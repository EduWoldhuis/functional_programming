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
  deriving (Show, Eq)
data Vector = Vector Float Float -- Vector dx dy is the 2d vector in the direction (dx, dy)
  deriving (Show, Eq)
data EqLine = EqLine Float Float Float -- EqLine a b c represents the line a * x + b * y + c = 0
  deriving (Show, Eq)
data VectLine = VectLine Point Vector -- VectLine p v represents the line through p in the direction v 
  deriving (Show, Eq)

class Line l where
  distance :: l -> Point -> Float
  vshift   :: l -> Float -> l
  
instance Line EqLine where
  distance (EqLine a b c) (Point x y) =
    abs(a*x+b*y+c)/sqrt(a*a+b*b)
  
  vshift (EqLine a b c) x =
    EqLine a b (c-x)


instance Line VectLine where
  -- distance (VectLine () v) (Point x y) =
  --   abs(a*x+b*y+c)/sqrt(a*a+b*b)
  
  vshift (VectLine (Point x y) (Vector vx vy)) s =
    VectLine (Point x (y+s)) (Vector vx vy)


-- 8

-- directed graph
class DGraph g where
  succs :: Eq a => g a -> a -> [a]

newtype PList k v = PList {keyValues :: [(k, v)]}
newtype SMPList k = SMPList (PList k [k])

data RoseTree l = RoseTree l [RoseTree l]
newtype FRoseTree l = FRoseTree [RoseTree l]

dgraph1SMPL = SMPList $ PList [(1, [2,3]), (2, []), (3, [2, 4]), (4, [3])]
dgraph1FET = FRoseTree [one] where
  one = RoseTree 1 [two, three]
  two = RoseTree 2 []
  three = RoseTree 3 [two, four]
  four = RoseTree 4 [three]


dgraph2SMPL = SMPList $ PList [('a', ['b','c','d','e']), ('b', ['d']), ('c', ['d','e']), ('d', ['e'])]
dgraph2FET = FRoseTree [a] where
  a = RoseTree 'a' [b, c, d, e]
  b = RoseTree 'b' [d]
  c = RoseTree 'c' [d, e]
  d = RoseTree 'd' [e]
  e = RoseTree 'e' []




