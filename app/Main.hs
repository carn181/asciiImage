module Main where
import GHC.IO.Encoding (TextEncoding(textEncodingName))
import GHC.Float
import System.Environment
import Data.Vector.Storable as Vector
import Data.List.Split.Internals
import Codec.Picture
import Codec.Picture.Extra

textRange :: [Char]
textRange = "$@B%8&WM#*oahkbdpqwmZO0QLCJUYXzcvunxrjft/\\|()1{}[]?-_+~<>i!lI;:,\"^`'."

imageToAscii :: String -> Image Pixel8 -> String
imageToAscii textRange img =
  unlines
  . chunksOf (imageWidth img)
  . toList
  $ Vector.map num2Char (imageData img)
  where
    num2Char x = textRange !! (fromIntegral x * Prelude.length textRange `div` 256)
    
rgbaToGray :: Image PixelRGBA8 -> Image Pixel8
rgbaToGray = pixelMap pixelAvg
  where pixelAvg (PixelRGBA8 r g b a) = round $ 0.299 * fromIntegral r + 0.587 * fromIntegral g + 0.114 * fromIntegral b

scaledImageHeight :: Int -> Image PixelRGBA8 -> Int
scaledImageHeight x img = round $ h/w * (int2Float x)
  where h = int2Float (imageHeight img)
        w = int2Float (imageWidth img)

renderImageFile :: Int -> String -> IO ()
renderImageFile x path =
  do 
    img <- readImage path
    case img of
      Left str -> print str
      Right img -> do
        let imgStandard = convertRGBA8 img
        putStrLn $ imageToAscii textRange (rgbaToGray $ scaleBilinear x (scaledImageHeight x imgStandard) imgStandard)

main = do
  args <- getArgs
  case args of
    [path,cols] -> renderImageFile (read cols :: Int) path
    _ -> putStrLn "USAGE: asciiImage path cols"
