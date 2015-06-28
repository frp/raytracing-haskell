module Geometry (
  Ray(Ray), direction, rayOrigin,
  Sphere(Sphere), normal, material,
  Material(Material), shininess, diffuseColor, specularColor,
  Vector3,
  intersect,
  perComponentMul, add, dot, mul, sub, normalize, neg, powComponents
) where

-- 2D vector
type Vector2 = (Double, Double)

-- 3D vector
type Vector3 = (Double, Double, Double)

-- Ray with origin and direction
data Ray = Ray {
  rayOrigin    :: Vector3,
  rayDirection :: Vector3
} deriving (Show)

direction :: Ray -> Vector3
direction ray = normalize (rayDirection ray)

data Material = Material {
  diffuseColor :: Vector3,
  specularColor:: Vector3,
  shininess    :: Double
} deriving (Show)

-- Sphere with centre and radius
data Sphere = Sphere {
  sphereOrigin :: Vector3,
  sphereRadius :: Double,
  material     :: Material
} deriving (Show)

vectorLength :: Vector3 -> Double
vectorLength (x, y, z) = sqrt(x^2 + y^2 + z^2)

-- produces a vector with the same origin and direction but length = 1
normalize :: Vector3 -> Vector3
normalize (x, y, z) = (x / len, y / len, z / len)
  where len = vectorLength (x, y, z)

add :: Vector3 -> Vector3 -> Vector3
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

sub :: Vector3 -> Vector3 -> Vector3
sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

dot :: Vector3 -> Vector3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

sqr :: Vector3 -> Double
sqr (x, y, z) = x^2 + y^2 + z^2

perComponentMul :: Vector3 -> Vector3 -> Vector3
perComponentMul (x1, y1, z1) (x2, y2, z2) = (x1 * x2, y1 * y2, z1 * z2)

normal :: Sphere -> Vector3 -> Vector3
normal (Sphere orig _ _) intersectionPoint = normalize (intersectionPoint `sub` orig)

mul :: Vector3 -> Double -> Vector3
mul (x, y, z) a = (a*x, a*y, a*z)

neg v = mul v (-1)

powComponents :: Vector3 -> Double -> Vector3
powComponents (x, y, z) p = (x**p, y**p, z**p)

-- returns the distance from ray origin to intersection point
-- or Nothing, if no intersection exists
intersect :: Ray -> Sphere -> Maybe Double
intersect ray sphere =
  let
    d_base = -(direction ray `dot` (rayOrigin ray `sub` sphereOrigin sphere))
    discriminant = (direction ray `dot` (rayOrigin ray `sub` sphereOrigin sphere)) ^ 2 - sqr (rayOrigin ray `sub` sphereOrigin sphere) + sphereRadius sphere ^ 2
  in
    if discriminant < 0
    then Nothing
    else
      let
        d1 = d_base + sqrt discriminant
        d2 = d_base - sqrt discriminant
      in
        if d1 < 0
        then Nothing
        else Just (if d2 >= 0 then min d1 d2 else d1)
