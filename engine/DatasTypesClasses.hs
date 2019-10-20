{-# LANGUAGE TemplateHaskell #-}

module DatasTypesClasses where

import           Codec.Picture
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.RWS.Strict
import           GHC.IO.Unsafe             (unsafePerformIO)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           System.Random

type Rotation = Int

type Game_ a = RWST Env () (GameState_ a) IO

data GameState_ a =
  GameState_
    { _stateWindowWidth  :: !Int
    , _stateWindowHeight :: !Int
    , _stateXScroll      :: !Double
    , _stateYScroll      :: !Double
    , _stateXDelta       :: !Double
    , _stateYDelta       :: !Double
    , _stateMouseDown    :: !Bool
    , _stateMouseClick   :: !Bool
    , _stateDragging     :: !Bool
    , _stateDragStartX   :: !Double
    , _stateDragStartY   :: !Double
    , _stateMouseX       :: !Double
    , _stateMouseY       :: !Double
    , _stateExit         :: !Bool
    , _activeScene       :: !(Scene_ a)
    , _scenes            :: ![Scene_ a]
    , _randomGenerator   :: StdGen
    , _gameInfo          :: !a
    }

data Env =
  Env
    { envEventsChan    :: TQueue Event
    , envWindow        :: !GLFW.Window
    , designResolution :: !(Int, Int)
    }

data Event
  = EventError !GLFW.Error !String
  | EventWindowPos !GLFW.Window !Int !Int
  | EventWindowSize !GLFW.Window !Int !Int
  | EventWindowClose !GLFW.Window
  | EventWindowRefresh !GLFW.Window
  | EventWindowFocus !GLFW.Window !Bool
  | EventWindowIconify !GLFW.Window !Bool
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos !GLFW.Window !Double !Double
  | EventCursorEnter !GLFW.Window !GLFW.CursorState
  | EventScroll !GLFW.Window !Double !Double
  | EventKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar !GLFW.Window !Char
  deriving (Show)

class Updatable a where
  update :: a b -> GameState_ b -> (a b, [GameState_ b -> GameState_ b])

data Scene_ a =
  Scene_
    { _sceneChildren :: [Node_ a]
    , _sceneId       :: String
    }

data Node_ a =
  Node_
    { _nodeParentTransform :: Transform
    , _nodeLocalTransform  :: Transform
    , _nodeGlobalTransform :: Transform
    , _nodeSprite          :: Maybe Sprite
    , _nodeCollider        :: Maybe Collider
    , _nodeChildren        :: [IO (Node_ a)]
    , _nodeUpdate          :: Node_ a -> GameState_ a -> Node_ a
    , _nodeGameStateUpdate :: Node_ a -> (GameState_ a -> GameState_ a)
    }

data Sprite =
  Sprite
    { _spriteSize    :: GL.Size
    , _spriteTexture :: !GL.DisplayList
    }

data Transform =
  Transform
    { _transformPosition'        :: GL.Vector3 Double
    , _transformRotation         :: Double
    , _transformScale            :: (Double, Double)
    , _transformAnchor           :: (Double, Double)
    , _transformSize             :: GL.Size
    , _transformPolarCoordinate' :: PolarCoordinate
    }
  deriving (Show)

data PolarCoordinate =
  PolarCoordinate
    { _polarR     :: Double
    , _polarAngle :: Double
    }
  deriving (Show)

data Collider
  = CircleCollider
      { circleColliderR :: !Double
      }
  | RectangleCollider
      { rectangleColliderSize :: !GL.Size
      }

makeLenses ''Transform

transformPosition :: Lens' Transform (GL.Vector3 Double)
transformPosition =
  lens
    _transformPosition'
    (\_transform position@(GL.Vector3 x y _) ->
       _transform
         { _transformPosition' = position
         , _transformPolarCoordinate' =
             PolarCoordinate
               { _polarR = sqrt (x * x + y * y)
               , _polarAngle =
                   if y /= 0 || x /= 0
                     then atan2 y x * 180.0 / pi
                     else 0
               }
         })

transformPolarCoordinate :: Lens' Transform PolarCoordinate
transformPolarCoordinate =
  lens
    _transformPolarCoordinate'
    (\_transform coordinate@(PolarCoordinate r angle) ->
       _transform
         { _transformPosition' =
             GL.Vector3
               (r * cos (angle * pi / 180))
               (r * sin (angle * pi / 180))
               ((\(GL.Vector3 _ _ z) -> z) (_transform & _transformPosition'))
         , _transformPolarCoordinate' = coordinate
         })

makeLenses ''Sprite

makeLenses ''Node_

makeLenses ''PolarCoordinate

makeLenses ''GameState_

isInCollision :: Node_ a -> Double -> Double -> Bool
isInCollision node x y =
  let collider = node ^. nodeCollider
      (GL.Vector3 x_ y_ _) = (node ^. (nodeGlobalTransform . transformPosition))
      (ax, ay) = node ^. (nodeGlobalTransform . transformAnchor)
      (GL.Size width height) = node ^. (nodeGlobalTransform . transformSize)
      (sx, sy) = node ^. (nodeGlobalTransform . transformScale)
   in case collider of
        Nothing -> False
        Just (CircleCollider r) -> (x - x_) ^ 2 + (y - y_) ^ 2 < r ^ 2
        Just (RectangleCollider (GL.Size cwidth cheight)) ->
          (x_ - fromIntegral width * ax < x) &&
          (x_ - fromIntegral width * ax + fromIntegral cwidth > x) &&
          (y_ - fromIntegral height * ay < y) && (y_ - fromIntegral height * ay + fromIntegral cheight > y)

-- && (x_ + fromIntegral width * ax * sx )
-- x_ + fromIntegral width * ax * sx
type MonitorInfo = (String, (Int, Int), (Int, Int), [GLFW.VideoMode])

class ToPixelRGBA8 a where
  toRGBA8 :: a -> PixelRGBA8

print_ :: Show a => a -> a
print_ t =
  unsafePerformIO
    (do putStrLn $ "____________________________"
        print t
        putStrLn "____________________________"
        return t)

printNode :: Node_ a -> Node_ a
printNode t =
  unsafePerformIO
    (do putStrLn "____________________________"
        print $ t & _nodeGlobalTransform
        putStrLn "____________________________"
        return t)
