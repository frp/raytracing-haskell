import Codec.Picture
import Geometry
import Scene

main = do
  let img = render (Scene 1920 1080 (pi/2) [Sphere (0, 0, 4.5) 0.5 (0.5, 0.5, 0.5)])
  savePngImage "test.png" (ImageRGB8 img)
  putStrLn "done"
