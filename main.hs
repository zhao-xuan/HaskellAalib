module Main where

import Prelude as P
import Graphics.Image as I hiding (invert)

invert :: (Num e, Fractional e) => Pixel RGB e -> Pixel RGB e
invert (PixelRGB r g b) = (PixelRGB avg avg avg)
    where avg = (r + b + g) / 3

main :: IO ()
main = do image <- readImageRGB VU "sunflower.jpg"
          let img = I.map invert image
          displayImage img