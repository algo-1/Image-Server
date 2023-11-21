module Shapes (
    Shape,
    Point,
    Vector,
    Transform,
    Drawing,
    Mask,
    MaskedDrawing,
    Image,
    image,
    maskedImage,
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
    shear,
    shearX,
    shearY,
    (<+>),
    inside,
    next,
    fairlyClose,
    nPolygon,
    getFirstColour,
    mask
)
where
import Codec.Picture (Pixel8)

-- Utilities

data Vector = Vector Double Double
    deriving (Show)

vector :: Double -> Double -> Vector
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

getX :: Vector -> Double
getX (Vector x y) = x

getY :: Vector -> Double
getY (Vector x y) = y

-- Shapes

type Point = Vector

point :: Double -> Double -> Point
point = vector

data Shape
    = Empty
    | Circle Colour
    | Square Colour
    | MandelbrotSet Colour
    | Rectangle Colour
    | Polygon Int Colour
    | Ellipse Colour
    deriving (Show)


empty :: Shape
empty = Empty

circle, square, mandelbrotSet, rectangle, ellipse :: Colour -> Shape
circle = Circle
square = Square
mandelbrotSet = MandelbrotSet
rectangle = Rectangle
ellipse = Ellipse

polygon :: Int -> Colour -> Shape
polygon = Polygon

triangle, pentagon, hexagon, heptagon, octagon, nonagon, decagon :: Colour -> Shape
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
    | Shear Vector
    deriving (Show)

identity :: Transform
identity = Identity

translate :: Vector -> Transform
translate = Translate

scale :: Vector -> Transform
scale = Scale

rotate :: Double -> Transform
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)

shear :: Vector -> Transform
shear = Shear

shearX :: Double -> Transform
shearX k = Shear (point k 0.0)

shearY :: Double -> Transform
shearY k = Shear (point 0.0 k)

(<+>) :: Transform -> Transform -> Transform
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity x = x
transform (Translate (Vector tx ty)) (Vector px py) = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty)) (Vector px py) = Vector (px / tx) (py / ty)
transform (Rotate m) p = invert m `mult` p
transform (Shear (Vector tx ty)) (Vector px py) = Vector (px - tx * py) (py - ty * px) 
transform (Compose t1 t2) p = transform t2 $ transform t1 p


-- Drawings

type Drawing = [(Transform, Shape)]
data Image = Image Drawing | MaskedImage (Drawing, Mask)
  deriving Show

image :: Drawing -> Image
image = Image
maskedImage :: (Drawing, Mask) -> Image
maskedImage = MaskedImage

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t, s) = insides (transform t p) s

insides :: Point -> Shape-> Bool
p `insides` Empty = False
p `insides` Circle _ = distance p <= 1
p `insides` Square _ = maxnorm p <= 1
p `insides` MandelbrotSet _ = all fairlyClose (take 1000 (mandelbrot p))
p `insides` Rectangle _ = insideRect p
p `insides` Polygon x _ = insidePolygon p $ nPolygon x
p `insides` Ellipse _ = insideEllipse p

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
nPolygon n = [rotatePoint (2 * pi / fromIntegral n * fromIntegral i) (point 0 1) | i <- [0 .. n-1]]

-- clockwise rotation
rotatePoint :: Double -> Point -> Point
rotatePoint theta (Vector x y) =
  point (x * cos theta + y * sin theta) (-x * sin theta + y * cos theta)

insideEllipse :: Point -> Bool
insideEllipse (Vector x y ) = x**2/2 + y**2 <= 1


-- colours
type Colour = (Pixel8, Pixel8, Pixel8)

getColour :: Shape -> Colour
getColour Empty = (0, 0, 0)
getColour (Circle c) = c
getColour (Square c) = c
getColour (MandelbrotSet c)= c
getColour (Rectangle c) = c
getColour (Polygon _ c) = c
getColour (Ellipse c) = c

inside2 :: Point -> (Transform, Shape) -> (Bool, Colour)
inside2 p (t, s) = (insides (transform t p) s, getColour s)

-- if point not in any shape colour black, else colour with the colour of the first shape it is in
getFirstColour :: Point -> Image -> Colour
getFirstColour p (Image d) = firstColour $ map (inside2 p) d

getFirstColour p (MaskedImage (d, m)) = insideMask m p $ firstColour $ map (inside2 p) d
                    
firstColour :: [(Bool, Colour)] -> Colour
firstColour ((False, _):xs) = firstColour xs
firstColour ((True, c):_)   = c
firstColour _ = (0, 0, 0)


-- masking -- the mask colour is ignored, when a mask is used, only points in the mask are displayed
type Mask = (Transform, Shape)
type MaskedDrawing = (Drawing, Mask)

mask :: Mask -> Drawing -> MaskedDrawing
mask m d = (d, m)

insideMask :: Mask -> Point -> Colour -> Colour
insideMask m p c = if inside1 p m then c else (0, 0, 0)
