## 24 bpp BMP to Negative

Simple haskell script that inverts (creates a negative of) a given `.bmp` image. The original miage must be:
1. 24 bpp pixel size
2. Uncompressed

|title|original|negative|
|-|-|-|
|bmp_24|![](images/bmp_24.bmP)|![](images/negative_bmp_24.bmP)|
|color wheel|![](images/color_wheel.bmp)|![](images/negative_color_wheel.bmp)|
|snail|![](images/snail.bmp)|![](images/negative_snail.bmp)|
|blackbuck|![](images/blackbuck.bmp)|![](images/negative_blackbuck.bmp)|
|marbles|![](images/MARBLES.BMP)|![](images/negative_MARBLES.BMP)|

Example images can be found in `/images` directory. `/images/unsupported` images contain examples of unsupported `.bmp` images.

## Executable Guide
* Install the executable using
  ```cabal install```
* Run the exectuable using
  ```bmp-negative {PATH_TO_BMP_IMAGE}```
  * `-h` flag for help message
* Image will be generated in the same directory as the original image.

## How it Works
* Script imports the `.bmp` image as a `ByteString` and converts is to a `[Byte]` or byte array.
* Split the byte array as `(Header,PixelData)`. 
  * Initially, the header data is parsed. Check whether application accepts the given `.bmp`.
  * Parse the `PixelData`.
    * We can consider it as an array of `Row`s.
    * Each `Row` is a byte array of `RGB` color bytes and `Padding` bytes.
    * The number of `Padding` bytes is `rowSize - 3 * width`, where `rowSize = div (width * 24 + 31) 32 * 4`
  * Relevent type alias' here:
    ```
    type BmpImage   = (Header,PixelData)
    type Header     = [Byte]
    type PixelData  = [Row]
    type Row        = (RGB,Padding)
    type RGB        = [Byte]
    type Padding    = [Byte]
    ```
* Invert the image. It's a simple operation of `255 - b`, where `b` is an RGB color byte.
* Write the new `BmpImage` as a new `.bmp`.