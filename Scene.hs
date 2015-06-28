module Scene (Scene(Scene), render) where

import Geometry
import Codec.Picture

data Scene = Scene {
  width   :: Int,
  height  :: Int,
  fovX    :: Double,
  spheres :: [Sphere]
} deriving (Show)

renderPlane :: Scene -> Double
renderPlane scene = fromIntegral (width scene) / (2 * tan(fovX scene / 2))

infinity = 1e300

traceRay :: Scene -> Ray -> Maybe Sphere
traceRay scene ray =
  let
    processSphere sphere (bestDistance, bestSphere) =
      let
        distance = ray `intersect` sphere
        returnValue Nothing = (bestDistance, bestSphere)
        returnValue (Just dist) = (dist, Just sphere)
      in
        returnValue distance
  in
    snd (foldr processSphere (infinity, Nothing) (spheres scene))

colorVectorToPixelColor :: Vector3 -> PixelRGB8
colorVectorToPixelColor (r, g, b) = PixelRGB8 (round (r*255)) (round (g * 255)) (round (b*255))

renderPixel :: Scene -> Int -> Int -> PixelRGB8
renderPixel (Scene width height fovX spheres) x y =
  let
    scene = Scene width height fovX spheres
    ray = Ray (0, 0, 0) (fromIntegral x - fromIntegral width / 2, fromIntegral height / 2 - fromIntegral y, renderPlane scene)
    shadeObject Nothing = PixelRGB8 0 0 0
    shadeObject (Just object) = colorVectorToPixelColor (color object)
    shape = scene `traceRay` ray
  in
    shadeObject shape

render :: Scene -> Image PixelRGB8
render scene = generateImage (renderPixel scene) (width scene) (height scene)
