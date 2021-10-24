module BMP where
import Data.Word(Word8)
import Data.Bits
import Data.List.Split

-- Alias for Word8. Word8 is essentially an 8bit unsigned Integral.
type Byte = Word8

-- Hexa-decimal to decimal Int for parsing Header
toInt :: [Byte] -> Int
toInt [] = 0
toInt [x] = fromIntegral x
toInt (x:xs) = fromIntegral x + (shift (toInt xs) 8)

-- Parsing Image given image width and pixel data row size
type BmpImage = (Header,PixelData)
type Header = [Byte]
type PixelData = [Row]
type Row = (RGB,Padding)
type RGB = [Byte]
type Padding = [Byte]

-- Split the Byte array at the offset
toBmpImage :: Int -> Int -> Int -> [Byte] -> BmpImage
toBmpImage width rowSize offSet = (\(h,p)->(h,toPixelData width rowSize p)) . splitAt offSet

-- Each row is rowSize bytes long
-- For each row, the padding is the last (rowSize - 3*width) bytes
-- i.e. the RGB bytes are the first 3*width bytes
toPixelData :: Int -> Int -> [Byte] -> PixelData
toPixelData width rowSize = map (splitAt $3*width) . chunksOf rowSize

-- simply concatenate everything
toByteArray :: BmpImage -> [Byte]
toByteArray = (\(h,p) -> h ++ concat (map (\(x,y)->x++y) p))

-- Negating enitre image
negateImage :: BmpImage -> BmpImage
negateImage (h,p) = (h, negatePixels p)

-- Negating the RGB values
negatePixels :: PixelData -> PixelData
negatePixels = map (\(x,y)-> (map ((-) 255) x , y))

-- Checking constraints on BMP
-- ByteArray to Either an Error or a BmpImage
parseImage :: [Byte] -> Either String BmpImage
parseImage image
  | header_field /= (66+77*256)   = Left "ERROR: Not a valid BMP file" 
  | compression_field /= 0        = Left "ERROR: BMP file is compressed"
  | pixel_size /= 24              = Left "ERROR: Pixel size should be 24 bits"
  | otherwise                     = Right $ toBmpImage width rowSize offSet image
  where header_field      = toInt [image!!0,image!!1]
        compression_field = toInt [image!!30,image!!31,image!!32,image!!33]
        pixel_size        = toInt [image!!28,image!!29]
        offSet            = toInt [image!!10, image!!11, image!!12, image!!13]
        width             = toInt [image!!18, image!!19, image!!20, image!!21]
        rowSize           = ceiling $ fromIntegral width * 3 / 32 * 4 