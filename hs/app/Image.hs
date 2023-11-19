module Image (vstackPng, hstackPng, hstackPngs) where

import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- | Stack two images vertically
vstack :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
vstack img1 img2 = generateImage pixelChooser width height
  where
    (width, height) = (imageWidth img1, imageHeight img1 + imageHeight img2)
    pixelChooser x y
      | y < imageHeight img1 = pixelAt img1 x y
      | otherwise = pixelAt img2 x (y - imageHeight img1)

vstackPng :: ByteString -> ByteString -> ByteString
vstackPng bs1 bs2 = BS.concat . BL.toChunks . encodePng $ vstack (toRGB8Png bs1) (toRGB8Png bs2)

toRGB8Png :: ByteString -> Image PixelRGB8
toRGB8Png = convertRGB8 . either error id . decodePng

hstack :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
hstack img1 img2 = generateImage pixelChooser width height
  where
    (width, height) = (imageWidth img1 + imageWidth img2, imageHeight img1)
    pixelChooser x y
      | x < imageWidth img1 = pixelAt img1 x y
      | otherwise = pixelAt img2 (x - imageWidth img1) y

hstackPng :: ByteString -> ByteString -> ByteString
hstackPng bs1 bs2 = BS.concat . BL.toChunks . encodePng $ hstack (toRGB8Png bs1) (toRGB8Png bs2)

hstackPngs :: [ByteString] -> ByteString
hstackPngs = BS.concat . BL.toChunks . encodePng . foldl1 hstack . map toRGB8Png
