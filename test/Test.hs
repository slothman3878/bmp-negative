module Main where

import Data.Either
import Test.Hspec
import qualified Data.ByteString as BS
import BMP

-- Convenient function
isIdentity :: Eq a => (a -> a) -> a -> Bool
isIdentity f x = x == (f x)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- correctly generates a negative image
-- compare generated negative with correct negative
-- NOTE we only need to compare the pixel data.
comparisonSpec :: BmpImage -> BmpImage -> Spec
comparisonSpec original correctAns = describe "comparison test:" $ do
  it "correctly generates a negative image" $
    equalPixels correctAns $ negateImage original
    -- Compare just the Pixel data of the given images
    where equalPixels :: BmpImage -> BmpImage -> Bool
          equalPixels (_,pixelData1) (_,pixelData2) = foldr (&&) True $ zipWith (\(colors1,_) (colors2,_) -> colors1==colors2) pixelData1 pixelData2

-- The negative images for comparison were created using gimp
main :: IO ()
main = do
  image1 <- BS.readFile "images/bmp_24.bmP"
  image2 <- BS.readFile "images/MARBLES.BMP"
  image3 <- BS.readFile "images/blackbuck.bmp"
  negative1 <- BS.readFile "test/correct/correct_negative_bmp_24.bmp"
  negative2 <- BS.readFile "test/correct/correct_negative_marbles.bmp"
  negative3 <- BS.readFile "test/correct/correct_negative_blackbuck.bmp"
  let images = map (fromRight ([],[]) . parseImage . BS.unpack) [image1,image2,image3]
  let negatives = map (fromRight ([],[]) . parseImage . BS.unpack) [negative1,negative2,negative3]
  hspec $ do
    comparisonSpec (images!!0) (negatives!!0)
    comparisonSpec (images!!1) (negatives!!1)
    comparisonSpec (images!!2) (negatives!!2)