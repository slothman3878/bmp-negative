module Main where
import System.IO
import System.Environment
import qualified Data.ByteString as BS
import BMP

main :: IO ()
main = do
  putStrLn "------------------------------------------------------------"
  args <- getArgs
  case parseArgs args of
    -- Error or Help Message
    Left msg -> putStrLn msg
    Right filePath -> do
      image <- BS.readFile filePath
      -- Now, parse the image
      case parseImage (BS.unpack image) of
        Left err -> putStrLn err
        Right a -> do
          let writePath = "new_bmp.bmp"
          BS.writeFile writePath $ BS.pack $ toByteArray $ negateImage a
          putStrLn "Success!!!"
          putStrLn $ "Negative image is saved to " ++ writePath
  putStrLn "------------------------------------------------------------"

-- Not the best design, but will do for now
parseArgs :: [String] -> Either String String
parseArgs [] = Left "ERROR: No Valid Argument Was Given.\nNeed to at least provide valid .bmp file.\nFor assistance, add help"
parseArgs (x:xs)
  | x == "help"       = Left "help has arrived!"
  | validFilePath x        = Right x
  | otherwise         = parseArgs xs

-- True if ends with ".bmp"
validFilePath :: String -> Bool
validFilePath [x1,x2,x3,x4] = (x1=='.')&&(x2=='b'||x2=='B')&&(x3=='m'||x3=='M')&&(x4=='p'||x4=='P')
validFilePath (x:xs) = validBmp xs

