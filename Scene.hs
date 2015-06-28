module Scene (Scene(Scene), render) where

import Geometry
import Codec.Picture

data Light = Light {
  lightOrigin :: Vector3,
  lightColor  :: Vector3
} deriving (Show)

data Scene = Scene {
  width        :: Int,
  height       :: Int,
  fovX         :: Double,
  spheres      :: [Sphere],
  ambientLight :: Vector3,
  lights       :: [Light]
} deriving (Show)

renderPlane :: Scene -> Double
renderPlane scene = fromIntegral (width scene) / (2 * tan(fovX scene / 2))

infinity = 1e300

traceRay :: Scene -> Ray -> Maybe Sphere
traceRay scene ray =
  let
    processSphere sphere (bestDistance, bestSphere) =
      case ray `intersect` sphere of
        Nothing   -> (bestDistance, bestSphere)
        Just dist -> if dist < bestDistance then (dist, Just sphere) else (bestDistance, bestSphere)
  in
    snd (foldr processSphere (infinity, Nothing) (spheres scene))

colorVectorToPixelColor :: Vector3 -> PixelRGB8
colorVectorToPixelColor (r, g, b) = PixelRGB8 (round (r*255)) (round (g * 255)) (round (b*255))

renderPixel :: Scene -> Int -> Int -> PixelRGB8
renderPixel (Scene width height fovX spheres ambientLight lights) x y =
  case scene `traceRay` ray of
    Nothing     -> PixelRGB8 0 0 0
    Just sphere -> colorVectorToPixelColor (color sphere `perComponentMul` ambientLight)
    where
      scene = Scene width height fovX spheres ambientLight lights
      ray = Ray (0, 0, 0) (fromIntegral x - fromIntegral width / 2, fromIntegral height / 2 - fromIntegral y, renderPlane scene)

render :: Scene -> Image PixelRGB8
render scene = generateImage (renderPixel scene) (width scene) (height scene)
