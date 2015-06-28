import Codec.Picture
import Geometry
import Scene

main = do
  let img = render (Scene 1920 1080 (pi/2)
                      [
                        Sphere (-0.5, 0, 2.5) 0.5 (Material (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) 1),
                        Sphere (0.5, 0, 2.2) 0.1 (Material (0.5, 0.5, 0.5) (0.5, 0.5, 0.5) 1)
                      ]
                    (0.1, 0.1, 0.1)
                      [
                        Light (1, 0, 2.1) (1, 1, 1)
                      ])
  savePngImage "test.png" (ImageRGB8 img)
  putStrLn "done"
