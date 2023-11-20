module Shapes (
    Shape,
    Point,
    Vector,
    Transform,
    Drawing,
    point,
    getX,
    getY,
    empty,
    circle,
    square,
    mandelbrotSet,
    rectangle,
    polygon,
    triangle, 
    pentagon, 
    hexagon, 
    heptagon, 
    octagon, 
    nonagon, 
    decagon,
    ellipse,
    identity,
    translate,
    rotate,
    scale,
    (<+>),
    inside,
    next,
    fairlyClose,
    nPolygon
)
where

-- Utilities

data Vector = Vector Double Double
    deriving (Show)

vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where
    k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
    deriving (Show)

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x

getY (Vector x y) = y

-- Shapes

type Point = Vector

point :: Double -> Double -> Point
point = vector

data Shape a
    = Empty
    | Circle
    | Square
    | MandelbrotSet
    | Rectangle
    | Polygon a
    | Ellipse
    deriving (Show)


empty, circle, square, mandelbrotSet, rectangle, ellipse :: Shape a
empty = Empty
circle = Circle
square = Square
mandelbrotSet = MandelbrotSet
rectangle = Rectangle
ellipse = Ellipse

polygon :: Int -> Shape Int
polygon = Polygon

triangle, pentagon, hexagon, heptagon, octagon, nonagon, decagon :: Shape Int
triangle = Polygon 3
pentagon = Polygon 5
hexagon = Polygon 6
heptagon = Polygon 7
octagon = Polygon 8
nonagon = Polygon 9
decagon = Polygon 10 


-- Transformations

data Transform
    = Identity
    | Translate Vector
    | Scale Vector
    | Compose Transform Transform
    | Rotate Matrix
    deriving (Show)

identity = Identity

translate = Translate

scale = Scale

rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity x = x
transform (Translate (Vector tx ty)) (Vector px py) = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty)) (Vector px py) = Vector (px / tx) (py / ty)
transform (Rotate m) p = invert m `mult` p
transform (Compose t1 t2) p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform, Shape Int)]


-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, Shape Int) -> Bool
inside1 p (t, s) = insides (transform t p) s

insides :: Point -> Shape Int -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm p <= 1
p `insides` MandelbrotSet = all fairlyClose (take 1000 (mandelbrot p))
p `insides` Rectangle = insideRect p
p `insides` Polygon x = insidePolygon p $ nPolygon x
p `insides` Ellipse = insideEllipse p

distance :: Point -> Double
distance (Vector x y) = sqrt (x ** 2 + y ** 2)

maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)

next :: Point -> Point -> Point
next (Vector u v) (Vector x y) = point (x * x - y * y + u) (2 * x * y + v)

fairlyClose :: Point -> Bool
fairlyClose (Vector u v) = (u * u + v * v) < 100

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (point 0 0)

insideRect :: Point -> Bool
insideRect (Vector x y ) = abs x <= 1 && abs y <= 0.5

insidePolygon :: Point -> [Point] -> Bool
insidePolygon p ps = all (uncurry (isSameSide p)) edges
  where
    edges = zip ps (tail ps ++ [head ps])

isSameSide :: Point -> Point -> Point -> Bool
isSameSide p p1 p2 =
  (getY p - getY p1) * (getX p2 - getX p1) - (getX p - getX p1) * (getY p2 - getY p1)  <= 0

nPolygon :: Int -> [Point]
nPolygon n = [rotatePoint (2 * pi / fromIntegral n * fromIntegral i) (point 1 0) | i <- [0 .. n-1]]

-- clockwise rotation
rotatePoint :: Double -> Point -> Point
rotatePoint theta (Vector x y) =
  point (x * cos theta + y * sin theta) (-x * sin theta + y * cos theta)

insideEllipse :: Point -> Bool
insideEllipse (Vector x y ) = x**2/2 + y**2 <= 1
