{-# LANGUAGE TypeApplications #-}

module FirstScene where

import           Arena
import           Control.Lens
import           Control.Monad.State       (State, evalState, execState, when)
import           DatasTypesClasses
import           Demo
import qualified Graphics.Rendering.OpenGL as GL
import           Mobs
import           Node
import           Scene                     (getSceneById)
import           Sprite
import           System.Random
import           Texture                   (createTexture, getTextureSize)
import           Transform

firstScene :: GL.Color4 Float -> IO (Scene_ GameInfo)
firstScene clearColor = do
  GL.clearColor GL.$= clearColor
  _caveButtonNode <- caveButtonNode
  _exitButtonNode <- exitButtonNode
  backpack <- backpackNode
  return Scene_ {_sceneChildren = [_caveButtonNode, _exitButtonNode, backpack], _sceneId = "firstScene"}

caveButtonSprite :: IO Sprite
caveButtonSprite = do
  let path = "assets/sprites/png_pictures/main_screen/cave_button.png"
  texture <- createTexture path
  size <- getTextureSize path
  return Sprite {_spriteSize = size, _spriteTexture = texture}

caveButtonNode :: IO Node
caveButtonNode = do
  sprite <- caveButtonSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformSize .= sprite ^. spriteSize
            transformAnchor .= (0.5, 0.5)
            transformPosition .= GL.Vector3 64 20 0)
    nodeCollider .= Just (RectangleCollider (sprite ^. spriteSize))
    nodeGameStateUpdate .= caveButtonGameStateUpdater
    nodeUpdate .= buttonsUpdater

exitButtonSprite :: IO Sprite
exitButtonSprite = do
  let path = "assets/sprites/png_pictures/main_screen/exit_button.png"
  texture <- createTexture path
  size <- getTextureSize path
  return Sprite {_spriteSize = size, _spriteTexture = texture}

exitButtonNode :: IO Node
exitButtonNode = do
  sprite <- exitButtonSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformSize .= (sprite ^. spriteSize)
            transformPosition .= GL.Vector3 64 50 1
            transformAnchor .= (0.5, 0.5))
    nodeCollider .= Just (RectangleCollider (sprite ^. spriteSize))
    nodeGameStateUpdate .= exitButtonUpdater
    nodeUpdate .= buttonsUpdater

caveButtonGameStateUpdater :: Node -> GameState -> GameState
caveButtonGameStateUpdater node gameState =
  let mousex = (gameState ^. stateMouseX)
      mousey = (gameState ^. stateMouseY)
   in if (gameState ^. stateMouseClick) && (isInCollision node mousex mousey)
        then gameState {_activeScene = getSceneById gameState "caveScene"}
        else gameState

exitButtonUpdater :: Node -> GameState -> GameState
exitButtonUpdater node gameState =
  let mousex = (gameState ^. stateMouseX)
      mousey = (gameState ^. stateMouseY)
   in if (gameState ^. stateMouseClick) && isInCollision node mousex mousey
        then gameState {_stateExit = True}
        else gameState

backpackSprite :: IO Sprite
backpackSprite = createSpriteWithPath "assets/sprites/png_pictures/main_screen/backpack.png"

backpackNode :: IO Node
backpackNode = do
  sprite <- backpackSprite
  return $ createNode $ do
    nodeSprite .=Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformSize .= sprite ^. spriteSize
            transformAnchor .= (0.5, 0.5)
            transformPosition .= GL.Vector3 115 60 1
            )
    nodeGameStateUpdate .= backpackButtonGameStateUpdater
    nodeUpdate .= buttonsUpdater
    nodeCollider .= Just (RectangleCollider (sprite ^. spriteSize))

backpackButtonGameStateUpdater :: Node -> GameState -> GameState
backpackButtonGameStateUpdater node gameState =
  let mousex = (gameState ^. stateMouseX)
      mousey = (gameState ^. stateMouseY)
   in if (gameState ^. stateMouseClick) && (isInCollision node mousex mousey)
        then gameState {_activeScene = getSceneById gameState "inventoryScene"}
        else gameState

buttonsUpdater :: Node -> GameState -> Node
buttonsUpdater node gameState =
  let (x, y) = (gameState ^. stateMouseX, gameState ^. stateMouseY)
   in if isInCollision node x y
        then execState (nodeLocalTransform . transformScale .= (1.1, 1.1)) node
        else execState (nodeLocalTransform . transformScale .= (1, 1)) node
