module FirstScene where

import           Arena
import           Control.Lens
import           Control.Monad.State       (State, evalState, execState, when)
import           DatasTypesClasses
import           Demo
import qualified Graphics.Rendering.OpenGL as GL
import           Node
import           Texture                   (createTexture, getTextureSize)
import           Transform

firstScene :: GL.Color4 Float -> IO (Scene_ GameInfo)
firstScene clearColor = do
  GL.clearColor GL.$= clearColor
  _caveButtonNode <- caveButtonNode
  _exitButtonNode <- exitButtonNode
  return Scene_ {_sceneChildren = [_caveButtonNode, _exitButtonNode]}

caveButtonSprite :: IO Sprite
caveButtonSprite = do
  let path = "assets/sprites/png_pictures/cave_button.png"
  texture <- createTexture path
  size <- getTextureSize path
  return Sprite {_spriteSize = size, _spriteTexture = texture}

caveButtonNode :: IO Node
caveButtonNode = do
  sprite <- caveButtonSprite
  return $
    createNode $ do
      nodeSprite .= Just sprite
      nodeLocalTransform .=
        createTransform
          (do transformSize .= sprite ^. spriteSize
              transformAnchor .= (0.5, 0.5)
              transformPosition .= GL.Vector3 64 20 0)

--      nodeGameStateUpdate .= caveButtonGameStateUpdater
--    Node_
--      { _nodeSprite = Just sprite
--      , _nodeUpdate = const
--      , _nodeGameStateUpdate = caveButtonGameStateUpdater
--      , _nodeChildren = []
--      , _nodeParentTransform = zeroTransform
--      , _nodeLocalTransform =
--          (transformWithPosition $ GL.Vector3 64 20 0)
--            {_transformSize = sprite ^. spriteSize, _transformAnchor = (0.5, 0.5)}
--      }
exitButtonSprite :: IO Sprite
exitButtonSprite = do
  let path = "assets/sprites/png_pictures/exit_button.png"
  texture <- createTexture path
  size <- getTextureSize path
  return Sprite {_spriteSize = size, _spriteTexture = texture}

exitButtonNode :: IO Node
exitButtonNode = do
  sprite <- exitButtonSprite
  return $
    createNode $ do
      nodeSprite .= Just sprite
      nodeLocalTransform .=
        createTransform
          (do transformSize .= (sprite ^. spriteSize)
              transformPosition .= GL.Vector3 64 40 1
              transformAnchor .= (0.5, 0.5))
      nodeCollider .= Just (RectangleCollider (sprite ^. spriteSize))
      nodeGameStateUpdate .= exitButtonUpdater

caveButtonGameStateUpdater :: Node -> (GameState -> GameState)
caveButtonGameStateUpdater node =
  \gameState ->
    if gameState ^. stateMouseDown
      then gameState {_activeScene = demoScene $ GL.Color4 0.3 0.3 0.3 1}
      else gameState

exitButtonUpdater :: Node -> (GameState -> GameState)
exitButtonUpdater node =
  \gameState ->
    let mousex = (gameState ^. stateMouseX)
        mousey = (gameState ^. stateMouseY)
     in if (gameState ^. stateMouseClick) && (isInCollision node mousex mousey)
          then gameState {_activeScene = demoScene $ GL.Color4 0.3 0.3 0.3 1}
          else gameState
