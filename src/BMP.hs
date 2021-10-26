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
type BmpImage   = (Header,PixelData)
type Header     = [Byte]
type PixelData  = [Row]
type Row        = (RGB,Padding)
type RGB        = [Byte]
type Padding    = [Byte]

-- Split the Byte array at the offset
-- Need to consider all list lengths for functional safety
-- The Pixel Data is rowSize * height bytes from the offset
toBmpImage :: Int -> Int -> Int -> [Byte] -> BmpImage
toBmpImage width height offSet = (\(h,p)->(h,toPixelData width rowSize $ take (rowSize * height) p)) . splitAt offSet
  where rowSize = div (width * 24 + 31) 32 * 4

-- Each row is rowSize bytes long
-- For each row, the padding is the last (rowSize - 3*width) bytes
-- i.e. the RGB bytes are the first 3*width bytes
toPixelData :: Int -> Int -> [Byte] -> PixelData
toPixelData width rowSize = map (splitAt $3*width) . chunksOf rowSize

-- simply concatenate everything
toByteArray :: BmpImage -> [Byte]
toByteArray (h,p) = (++) h $ concat $ map (uncurry (++)) p

-- Negating the RGB values
negatePixels :: PixelData -> PixelData
negatePixels = map (\(x,y)-> (map complement x , y))

-- Negating enitre image
negateImage :: BmpImage -> BmpImage
negateImage (h,p) = (h, negatePixels p)

-- Checking constraints on BMP
-- ByteArray to Either an Error or a BmpImage
-- Functionally problematic. Consider all List lengths
-- The length limit of 54 is somewhat of an arbitrary value. From what I understand, DIB header ends at 54th byte
parseImage :: [Byte] -> Either String BmpImage
parseImage image
  | length image < 54             = Left "byte array shorter than 54: Not long enough to be parsed\n Check whether your image is a valid bmp image"
  | header_field /= (66+77*256)   = Left "wrong header field: Check whether your image is a valid bmp"
  | compression_field /= 0        = Left "compressed BMP: can only accept uncompressed BMP"
  | bpp /= 24                     = Left "not 24bpp: can only support 24bpp images"
  | otherwise                     = Right $ toBmpImage width height offSet image
  where header_field      = toInt [image!!0,image!!1]
        compression_field = toInt [image!!30,image!!31,image!!32,image!!33]
        bpp               = toInt [image!!28,image!!29]
        offSet            = toInt [image!!10, image!!11, image!!12, image!!13]
        width             = toInt [image!!18, image!!19, image!!20, image!!21]
        height            = toInt [image!!22, image!!23, image!!23, image!!24]