module Main where

import Control.Monad (msum)
import Happstack.Server
import System.Process -- (callProcess)
import System.FilePath ((</>))


tensorflowBase :: FilePath -> FilePath
tensorflowBase p = "/home/pi/tensorflow" </> p

camera :: FilePath
camera = tensorflowBase "tensorflow/contrib/pi_examples/camera/gen/bin/camera"


raspistill :: FilePath
raspistill = "/usr/bin/raspistill"

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "cam.jpg" $ serveFile (asContentType "image/jpeg") "cam.jpg"
  , ok $ toResponse "hello, world!"
  ]
