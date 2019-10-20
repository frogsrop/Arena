{-# LANGUAGE TemplateHaskell #-}

module Mobs where

import           Control.Lens
import           DatasTypesClasses
import           Sprite
import           System.Random     (getStdRandom, randomR)

data MobInfo =
  MobInfo
    { _mobInfoHp            :: !Int
    , _mobInfoAtk           :: !Int
    , _mobInfoDef           :: !Int
    , _mobInfoSprite        :: !Sprite
    , _mobInfoAtackCooldown :: !Int
    }

slime :: IO MobInfo
slime = do
  sprite <- slimeSprite
  return
    MobInfo
      { _mobInfoHp = 10
      , _mobInfoAtk = 1
      , _mobInfoDef = 5
      , _mobInfoSprite = sprite
      , _mobInfoAtackCooldown = 100
      }

slimeSprite :: IO Sprite
slimeSprite = createSpriteWithPath "assets/sprites/png_pictures/cave/slime.png"

makeLenses ''MobInfo
