import Codec.Picture
import Geometry
import Scene

main = do
  let img = render (Scene 1920 1080 (pi/2) [Sphere (0, 0, 4.5) 0.5 (0.5, 0.5, 0.5), Sphere (0, 0, 3.5) 0.5 (1, 1, 1)] (0, 0, 0) [])
  savePngImage "test.png" (ImageRGB8 img)
  putStrLn "done"
