module Arena where

import           DatasTypesClasses

data GameInfo =
  GameInfo
    {
    }

type Node = Node_ GameInfo

type Scene = Scene_ GameInfo

type Game = Game_ GameInfo

type GameState = GameState_ GameInfo
