{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Texture
  ( createBMPTexture
  , createTexture
  , getTextureSize
  ) where

import           Codec.BMP                 (BMP, bmpDimensions, readBMP,
                                            unpackBMPToRGBA32)
import           Codec.Picture
import qualified Data.ByteString           as B
import           GHC.Word                  (Word8)
import qualified Graphics.Rendering.OpenGL as GL
import DatasTypesClasses

createBMPTexture :: String -> IO GL.DisplayList
createBMPTexture path =
  GL.defineNewList GL.Compile $ do
    GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
    drawCharacter
  where
    drawCharacter = do
      GL.shadeModel GL.$= GL.Flat
      Right bmp <- readBMP path
      let rgba = unpackBMPToRGBA32 bmp
      walk rgba 0 bmp
      where
        walk :: B.ByteString -> Int -> BMP -> IO ()
        walk rgba cur bmp =
          if rgba /= B.empty
            then do
              let r = B.head rgba
                  g = B.head $ B.tail rgba
                  b = B.head $ B.tail $ B.tail rgba
                  rgbf = toFloatRGB (r, g, b)
              GL.renderPrimitive GL.QuadStrip $ do
                let color = GL.Color4 @Float (tfst rgbf) (tsnd rgbf) (tthd rgbf) 1
                if (1, 1, 1) == rgbf
                  then GL.color color
                  else do
                    GL.color color
                    let coordinate = getCoordinate cur
                    GL.vertex $ uncurry GL.Vertex2 coordinate
                    GL.vertex $ GL.Vertex2 (fst coordinate) (snd coordinate + 1)
                    GL.vertex $ GL.Vertex2 (fst coordinate + 1) (snd coordinate)
                    GL.vertex $ GL.Vertex2 (fst coordinate + 1) (snd coordinate + 1)
              walk (B.tail $ B.tail $ B.tail $ B.tail rgba) (cur + 1) bmp
            else putStr "Done"
          where
            (fwidth , fheight ) = (fromIntegral width, fromIntegral height)
            (width, height) = bmpDimensions bmp
            correct :: (Int, Int) -> (Float, Float)
            correct (x, y) = (fromIntegral x - (fwidth / 2), fromIntegral y - (fheight / 2))
            getCoordinate :: Int -> (Float, Float)
            getCoordinate num = correct (num `mod` width, num `div` width)

toFloatRGB :: (Word8, Word8, Word8) -> (Float, Float, Float)
toFloatRGB (r, g, b) = (realToFrac r / 255, realToFrac g / 255, realToFrac b / 255)

tfst :: (a, b, c) -> a
tfst (a, _, _) = a

tsnd :: (a, b, c) -> b
tsnd (_, b, _) = b

tthd :: (a, b, c) -> c
tthd (_, _, c) = c

instance ToPixelRGBA8 PixelYA8 where
  toRGBA8 (PixelYA8 l a) = PixelRGBA8 l l l a

instance ToPixelRGBA8 PixelRGB8 where
  toRGBA8 (PixelRGB8 r g b) = PixelRGBA8 r g b 255

instance ToPixelRGBA8 PixelRGBA8 where
  toRGBA8 = id

fromDynamicImage :: DynamicImage -> Image PixelRGBA8
fromDynamicImage (ImageYA8 img)   = pixelMap toRGBA8 img
fromDynamicImage (ImageRGB8 img)  = pixelMap toRGBA8 img
fromDynamicImage (ImageRGBA8 img) = img
fromDynamicImage _                = error "Unsupported format"

loadImage :: FilePath -> IO (Image PixelRGBA8)
loadImage path = do
  image <- readImage path
  case image of
    Left msg -> fail msg
    Right image' -> return (fromDynamicImage image')

createTexture :: FilePath -> IO GL.DisplayList
createTexture path = do
  image <- loadImage path
  GL.defineNewList GL.Compile $ do
    GL.colorMaterial GL.$= Just (GL.Front, GL.AmbientAndDiffuse)
    GL.shadeModel GL.$= GL.Flat
    renderPixels image
  where
    renderPixels :: Image PixelRGBA8 -> IO ()
    renderPixels image@(Image w h _) = do
      let coords = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
      evaluateCoords coords
      where
        evaluateCoords :: [(Int, Int)] -> IO ()
        evaluateCoords [] = putStrLn "Done"
        evaluateCoords ((x, y):xs) = do
          evaluateCoords xs
          GL.renderPrimitive GL.QuadStrip $ do
            GL.color (colorAt image x y)
            GL.vertex (GL.Vertex2 @Float (fromIntegral x) (fromIntegral y))
            GL.vertex (GL.Vertex2 @Float (fromIntegral x + 1) (fromIntegral y))
            GL.vertex (GL.Vertex2 @Float (fromIntegral x) (fromIntegral y + 1))
            GL.vertex (GL.Vertex2 @Float (fromIntegral x + 1) (fromIntegral y + 1))

getTextureSize :: FilePath -> IO GL.Size
getTextureSize path = do
  (Image w h _) <- loadImage path
  return (GL.Size (fromIntegral w) (fromIntegral h))

colorAt :: Image PixelRGBA8 -> Int -> Int -> GL.Color4 Float
colorAt image x y =
  let PixelRGBA8 r g b a = pixelAt image x y
   in GL.Color4 (fromIntegral r / 255.0) (fromIntegral g / 255.0) (fromIntegral b / 255.0) (fromIntegral a / 255.0)

