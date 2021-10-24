module Main where
import System.IO  
import qualified Data.ByteString as BS
import Data.Word(Word8)
import Data.Bits
import Data.List.Split

main :: IO ()
main = do
  -- read the raw .bmp file
  image <- BS.readFile "new_bmp.bmp"
  case parseImage (BS.unpack image) of
    Left err -> print err
    Right a -> BS.writeFile "new_bmp.bmp" $ BS.pack $ toByteArray $ negateImage a

parseImage :: [Byte] -> Either String BmpImage
parseImage image
  | header_field /= (66+77*256)       = Left "Not a valid BMP file" 
  | compression_field /= 0            = Left "BMP file is compressed"
  | pixel_size /= 24                  = Left "Pixel size should be 24 bits"
  | otherwise                         = Right $ toBmpImage image
  where header_field = toInt [image!!0,image!!1]
        compression_field = toInt [image!!30,image!!31,image!!32,image!!33]
        pixel_size = toInt [image!!28,image!!29]
        width = toInt

negateImage :: BmpImage -> BmpImage
negateImage image = 

-- HexNum to Int for parsing Header
toInt :: [Byte] -> Int
toInt [] = 0
toInt [x] = fromIntegral x
toInt (x:xs) = fromIntegral x + (shift (toInt xs) 8)

-- Parsing Image given image width and pixel data row size
type Byte = Word8
type BmpImage = [([Byte],[Byte])] --[([RGB Byte],[Padding Byte])]

-- Each row is rowSize bytes long
-- For each row, the padding is the last (rowSize - 3*width) bytes
toBmpImage :: Int -> Int -> [Byte] -> BmpImage
toBmpImage width rowSize = map (splitAt $3*width) . chunksOf rowSize

-- simply concatenate everything
toByteArray :: BmpImage -> [Byte]
toByteArray = concat . map (\(x,y)-> x++y)

-- Negating the RGB values
negateImage :: BmpImage -> BmpImage
negateImage = map (\(x,y)-> (map ((-) 255) x , y))