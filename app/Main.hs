module Main where
import System.IO  
import qualified Data.ByteString as BS
import Data.Word(Word8)
import Data.Bits
import Data.List.Split

main :: IO ()
main = do
  -- read the raw .bmp file
  raw <- BS.readFile "new_bmp.bmp"
  let content = BS.unpack raw
  -- get the index where pixels start
  let offSet = toInt [content!!10, content!!11, content!!12, content!!13]
  -- get header bytes
  let (header,pixelData) = splitAt offSet content
  -- apply constraints on header

  -- get image width
  let width = toInt [content!!18, content!!19, content!!20, content!!21]
  -- get rowSize of pixelData
  let rowSize = (*) 4 $ ceiling $ (24 * (fromIntegral width)) / 32
  -- parse pixel data and get negative colors
  let negativePixels = toByteArray $ negateImage $ parseImage width rowSize pixelData
  -- write to bmp file
  BS.writeFile "new_bmp.bmp" $ BS.pack $ header ++ negativePixels
  print "success!"

-- HexNum to Int for parsing Header
toInt :: [Word8] -> Int
toInt [] = 0
toInt [x] = fromIntegral x
toInt (x:xs) = fromIntegral x + (shift (toInt xs) 8)

-- Parsing Image given image width and pixel data row size
type Byte = Word8
type BmpImage = [([Byte],[Byte])] --[([RGB Byte],[Padding Byte])]

-- Each row is rowSize bytes long
-- For each row, the padding is the last (rowSize - 3*width) bytes
parseImage :: Int -> Int -> [Word8] -> BmpImage
parseImage width rowSize = map (splitAt $3*width) . chunksOf rowSize

-- simply concatenate everything
toByteArray :: BmpImage -> [Word8]
toByteArray = concat . map (\(x,y)-> x++y)

-- Negating the RGB values
negateImage :: BmpImage -> BmpImage
negateImage = map (\(x,y)-> (map ((-) 255) x , y))