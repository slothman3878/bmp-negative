module Main where
import System.IO  
import qualified Data.ByteString as BS
import Data.Word(Word8)
import Data.Bits
import Data.List.Split

main :: IO ()
main = do
  -- read the raw .bmp file
  raw <- BS.readFile "MARBLES.BMP"
  let content = BS.unpack raw
  -- get the index where pixels start
  let offSet = toInt [content!!10, content!!11, content!!12, content!!13]
  -- get header bytes
  let header = take offSet content
  let pixelData = drop offSet content
  -- apply constraints on header

  -- get image width
  let width = toInt [content!!18, content!!19, content!!20, content!!21]
  print width
  let rowSize = (*) 4 $ ceiling $ (24 * (fromIntegral width)) / 32
  print rowSize
  -- negate pixels
  let negativePixels = map negatePixel (toPixels rowSize width pixelData)
  -- write to bmp file
  BS.writeFile "new_bmp.bmp" $ BS.pack $ header ++ negativePixels
  print "success!"

-- HexNum to Int for parsing Header
toInt :: [Word8] -> Int
toInt [x] = fromIntegral x
toInt (x:xs) = fromIntegral x + (shift (toInt xs) 8)

-- Given rowSize and width, return parsed Pixels
toPixels :: Int -> Int -> [Word8] -> Pixels
toPixels rowSize width pixels = concat (map (parseRow width) (chunksOf rowSize pixels))

-- Parsing a Single Row
parseRow :: Int -> [Word8] -> Pixels
parseRow width = (\(x,y) -> map RGB x ++ (map (\z->Padding) y)) . (splitAt $ 4*width)

negatePixel :: MaybeColor -> Word8
negatePixel pixel = case pixel of
  RGB a -> 255 - a
  Padding -> 0

-- A byte in the pixel data of a bmp is either an RGB value or a Padding
data MaybeColor = RGB Word8 | Padding
type Pixels = [MaybeColor]