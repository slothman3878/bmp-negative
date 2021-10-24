## 24 bit BMP to Negative
##### Written in Haskell

Simple haskell script that creates a negative of a given `.bmp` file. Supported `.bmp` are limited to
1. 24bit pixels
2. Non-compressed

|original|negative|
|-------|-------|
|![](blackbuck.bmp)|![](blackbuck.bmp)|

## How it Works
* Script imports the `.bmp` image as a `ByteString` and converts is to a `[Byte]` or byte array.
* The header data is parsed. If the 

## TODO
- [ ] Custom flags &ndash; e.g. `-help`