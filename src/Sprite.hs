module Sprite where

import           DatasTypesClasses
import           Texture

createSpriteWithPath :: FilePath -> IO Sprite
createSpriteWithPath path = do
  texture <- createTexture path
  size <- getTextureSize path
  return $ Sprite size texture
