module Scene (
  Scene(Scene), render
  , Light(Light)
  ) where

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

traceRay :: Scene -> Ray -> Maybe (Double, Sphere)
traceRay scene ray =
  let
    processSphere :: Sphere -> Maybe (Double, Sphere) -> Maybe (Double, Sphere)
    processSphere sphere result =
      case ray `intersect` sphere of
        Nothing   -> result
        Just dist -> case result of
                      Nothing -> Just (dist, sphere)
                      Just (bestDist, _) -> if dist < bestDist then Just (dist, sphere) else result
  in
    foldr processSphere Nothing (spheres scene)

applyShading scene ray distance shape =
  let
    intersectionPoint = (direction ray `mul` distance) `add` rayOrigin ray
    intersectionNormal= normal shape intersectionPoint

    processLight lightSource accumulated =
      let
        lightSourceDir = normalize (lightOrigin lightSource `sub` intersectionPoint)
        obstacle = traceRay scene (Ray (intersectionPoint `add` (lightSourceDir `mul` 0.05)) lightSourceDir)
      in
        case obstacle of
          Just _ -> accumulated
          Nothing ->
            let
              diffuseDotProduct = lightSourceDir `dot` intersectionNormal
              diffuse = if diffuseDotProduct > 0 then lightColor lightSource `mul` diffuseDotProduct else (0, 0, 0)

              r = intersectionNormal `mul` 2 `mul` diffuseDotProduct `sub` lightSourceDir
              viewerDir = neg (direction ray)
              specularDotProduct = viewerDir `dot` r

              specular = if specularDotProduct > 0 && diffuseDotProduct > 0
                          then powComponents ((lightColor lightSource) `mul` (viewerDir `dot` r)) (shininess (material shape))
                          else (0, 0, 0)
            in
              accumulated `add` (diffuse `perComponentMul` (diffuseColor (material shape))) `add` (specular `perComponentMul` (specularColor (material shape)))
  in
    foldr processLight (ambientLight scene) (lights scene)

colorVectorToPixelColor :: Vector3 -> PixelRGB8
colorVectorToPixelColor (r, g, b) = PixelRGB8 (round (min r 1 * 255)) (round (min g 1 * 255)) (round (min b 1 * 255))

renderPixel :: Scene -> Int -> Int -> PixelRGB8
renderPixel (Scene width height fovX spheres ambientLight lights) x y =
  case scene `traceRay` ray of
    Nothing             -> PixelRGB8 0 0 0
    Just (dist, sphere) -> colorVectorToPixelColor (applyShading scene ray dist sphere)
    --(diffuseColor (material sphere) `perComponentMul` ambientLight)
    where
      scene = Scene width height fovX spheres ambientLight lights
      ray = Ray (0, 0, 0) (fromIntegral x - fromIntegral width / 2, fromIntegral height / 2 - fromIntegral y, renderPlane scene)

render :: Scene -> Image PixelRGB8
render scene = generateImage (renderPixel scene) (width scene) (height scene)
