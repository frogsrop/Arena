{-# LANGUAGE Rank2Types #-}

module InventoryScene where

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

inventoryScene :: GL.Color4 Float -> IO Scene
inventoryScene clearColor = do
  GL.clearColor GL.$= clearColor
  back <- backNode
  exit <- exitNode
  return Scene_ {_sceneChildren = [back, exit], _sceneId = "inventoryScene"}

backSprite :: IO Sprite
backSprite = createSpriteWithPath "assets/sprites/png_pictures/inventory/back.png"

backNode :: IO Node
backNode = do
  sprite <- backSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 0 0 0.3
            transformSize .= sprite ^. spriteSize)
    nodeChildren .= [gridNode]

gridSprite :: IO Sprite
gridSprite = createSpriteWithPath "assets/sprites/png_pictures/inventory/grid.png"

gridNode :: IO Node
gridNode = do
  sprite <- gridSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformSize .= sprite ^. spriteSize
            transformAnchor .= (0, 0)
            transformPosition .= GL.Vector3 5 37 0 --37
         )
    nodeUpdate .= gridUpdate
    nodeChildren .= map generateNthChildForGrid [0 .. 28]

--generateChildrenForGrid ::
generateNthChildForGrid :: Int -> IO Node
generateNthChildForGrid n =
  let y = n `div` 4
      x = n `mod` 4
   in return $ createNode $ do
        nodeLocalTransform .=
          createTransform (transformPosition .= GL.Vector3 (fromIntegral (x * 15) + 2) (fromIntegral (y * 15) + 2) 0.1)
        nodeUpdate .=
          (\node gameState ->
             let sprites = gameState ^. gameInfo . gameInfoPlayerInfo . playerInfoInventory
                 sprite = nth n sprites
                 nodeWithSprite = set nodeSprite sprite node
              in set (nodeLocalTransform.transformSize) (case sprite of Just s  -> s ^. spriteSize; Nothing -> GL.Size 0 0)
                                                                      nodeWithSprite)
   where
    nth _ [] = Nothing
    nth a (x:xs)
        | a == 0 = Just x
        | otherwise = nth (a - 1) xs

gridUpdate :: Node -> GameState -> Node
gridUpdate node gameState =
  let (GL.Vector3 x y z) = (node ^. nodeLocalTransform . transformPosition)
   in execState (nodeLocalTransform . transformPosition .= GL.Vector3 x (fixY y) z) node
  where
    fixY y
      | (y + gameState ^. stateYScroll * 2) > 37 = 37
      | (y + gameState ^. stateYScroll * 2) < (-37) = -37
      | otherwise = y + gameState ^. stateYScroll * 2

exitSprite :: IO Sprite
exitSprite = createSpriteWithPath "assets/sprites/png_pictures/inventory/exit.png"

exitNode :: IO Node
exitNode = do
  sprite <- exitSprite
  return $ createNode $ do
    nodeSprite .=Just sprite
    nodeLocalTransform .=
      createTransform
        (do transformSize .= sprite ^. spriteSize
            transformAnchor .= (0.5, 0.5)
            transformPosition .= GL.Vector3 117 11 0.4
            )
    nodeGameStateUpdate .= exitButtonGameStateUpdater
    nodeUpdate .= buttonsUpdater
    nodeCollider .= Just (RectangleCollider (sprite ^. spriteSize))

exitButtonGameStateUpdater :: Node -> GameState -> GameState
exitButtonGameStateUpdater node gameState =
  let mousex = (gameState ^. stateMouseX)
      mousey = (gameState ^. stateMouseY)
   in if (gameState ^. stateMouseClick) && (isInCollision node mousex mousey)
        then gameState {_activeScene = getSceneById gameState "firstScene"}
        else gameState

buttonsUpdater :: Node -> GameState -> Node
buttonsUpdater node gameState =
  let (x, y) = (gameState ^. stateMouseX, gameState ^. stateMouseY)
   in if isInCollision node x y
        then execState (nodeLocalTransform . transformScale .= (1.1, 1.1)) node
        else execState (nodeLocalTransform . transformScale .= (1, 1)) node
