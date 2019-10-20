{-# LANGUAGE TemplateHaskell #-}

module Arena where

import           Control.Lens
import           DatasTypesClasses
import           Mobs

data GameInfo =
  GameInfo
    { _gameInfoCurrentFight :: !(Maybe (Mob, Player))
    , _gameInfoMobsList     :: ![MobInfo]
    , _gameInfoPlayerInfo   :: !PlayerInfo
    }

data Mob =
  Mob
    { _mobInfo          :: !MobInfo
    , _mobCurrentHP     :: !Double
    , _mobAtackCooldown :: !Int
    , _mobRandomSeed    :: !Int
    }

data Player =
  Player
    { _playerCurrentHP     :: !Int
    , _playerAtackCooldown :: !Int
    }

data PlayerInfo =
  PlayerInfo
    { _playerInfoHP            :: !Int
    , _playerInfoAtk           :: !Int
    , _playerInfoDef           :: !Int
    , _playerInfoAtackCooldown :: !Int
    , _playerInfoInventory     :: ![Sprite]
    }

makeLenses ''GameInfo

makeLenses ''PlayerInfo

makeLenses ''Player

makeLenses ''Mob

type Node = Node_ GameInfo

type Scene = Scene_ GameInfo

type Game = Game_ GameInfo

type GameState = GameState_ GameInfo
