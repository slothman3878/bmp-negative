module Main where
import Control.Exception
import System.Environment
import qualified Data.ByteString as BS
import Data.List.Split
import BMP

main :: IO ()
main = do
  putStrLn "------------------------------------------------------------"
  putStrLn "Generate a Negative from a BMP file "
  putStrLn "------------------------------------------------------------"
  args <- getArgs
  case parseArgs args of
    -- Error or Help Message
    Left err -> putStrLn err
    Right filePath -> do
      readResult <- try $ BS.readFile filePath :: IO (Either SomeException BS.ByteString)
      case readResult of
        Left err -> putStrLn $ "ERROR: " ++ show err
        Right unpackedImage -> do
          -- Now, parse the image. Parse [Byte] as BmpImage 
          case parseImage (BS.unpack unpackedImage) of
            Left err -> putStrLn $ "ERROR: " ++ err
            Right parsedImage -> do
              let (name,path) = parseFileName filePath
              let writePath = path ++ "/negative_" ++ name
              let negative = negateImage parsedImage
              BS.writeFile writePath $ BS.pack $ toByteArray negative
              putStrLn "Success!!!"
              putStrLn $ "Negative image is saved to " ++ writePath
  putStrLn "------------------------------------------------------------"

-- Not the best design. Should ask about this later...
parseArgs :: [String] -> Either String String
parseArgs [] = Left "ERROR: No Valid Argument Was Given.\nNeed to at provide a valid .bmp file.\nFor assistance, apply the -h flag."
parseArgs (x:xs)
  | x == "-h"         = Left "Help has arrived!\nRun the exectable with path to the bmp image you want to negate."
  | validFilePath x   = Right x
  | otherwise         = parseArgs xs

-- Extract just the file name from the file path
-- (fileName,filePath)
parseFileName :: String -> (String,String)
parseFileName = mapTuple reverse . break ('/' ==) . reverse

-- For convenience
mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- True if ends with ".bmp" or equivalent. I have checked that capitalization of the letters doesn't matter.
validFilePath :: String -> Bool
validFilePath [] = False
validFilePath [x1,x2,x3,x4] = (x1=='.')&&(x2=='b'||x2=='B')&&(x3=='m'||x3=='M')&&(x4=='p'||x4=='P')
validFilePath (_:xs) = validFilePath xs