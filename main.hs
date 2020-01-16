module Main where

import Prelude as P
import Graphics.Image as I hiding (invert)

type Length = Int
type Width = Int

divideRectangle :: (Num e, Fractional e) => (Width, Length) -> Image arr cs e -> [[Pixel RGB e]]
divideRectangle (w, h) image
  = map crop [(wblock, hblock) | wblock <- [0..wcount - 1], hblock <- [0..hcount - 1]]
      where (wcount, hcount) = (div (cols image) w, div (rows image) h)
            crop (wfrom, hfrom) = map (\x -> index image x) list
                where list = [(i, j) | i <- [w * wfrom..(w+1) * wfrom], j <- [h * hfrom..(h+1) * hfrom]]
        
grayscale :: (Num e, Fractional e) => Pixel RGB e -> Pixel RGB e
grayscale (PixelRGB r g b)
  = (PixelRGB avg avg avg)
      where avg = (0.3 * r + 0.59 * g + 0.11 * b)
            brightness = (0.299 * r + 0.587 * g + 0.114 * b)

contrast :: (Num e, Fractional e) => Image arr cs e -> [[Pixel RGB e]] -> Int -> [[Pixel RGB e]]
contrast image pixels contra
  = map (\x -> map grayscale x) pixels
      where brightness (PixelRGB r g b) = (0.299 * r + 0.587 * g + 0.114 * b)
            dlist = map (\x -> div (sum (map brightness x)) (length x)) pixels
            clist = map (\x -> ((x - 0.5) * contra) + 0.5) dlist

main :: IO ()
main = do image <- readImageRGB VU "sunflower.jpg"
          let img = I.map invert image
          displayImage img