module Demo where

import           Arena
import           Control.Lens
import           Control.Monad.State       (execState, when)
import           DatasTypesClasses
import qualified Graphics.Rendering.OpenGL as GL
import           Node
import           Sprite
import           Texture
import           Transform

demoScene :: GL.Color4 Float -> IO Scene
demoScene clearColor = do
  GL.clearColor GL.$= clearColor
  _sunNode <- sunNode
  return Scene_ {_sceneChildren = [_sunNode], _sceneId = "demoScene"}

sunSprite = createSpriteWithPath "assets/demo_sprites/sun.png"

mercurySprite = createSpriteWithPath "assets/demo_sprites/mercury.png"

venusSprite = createSpriteWithPath "assets/demo_sprites/venus.png"

earthSprite = createSpriteWithPath "assets/demo_sprites/earth.png"

moonSprite = createSpriteWithPath "assets/demo_sprites/moon.png"

sunNode = do
  sprite <- sunSprite
  return $ createNode $ do
    nodeSprite .= Just sprite
    nodeUpdate .= modifier
    nodeChildren .= [mercuryNode, venusNode, earthNode]
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 64 32 0
            transformAnchor .= (0.5, 0.5)
            transformSize .= sprite ^. spriteSize)

mercuryNode :: IO Node
mercuryNode = do
  sprite <- mercurySprite
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 13 0 0.6
            transformAnchor .= (0.5, 0.5)
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeUpdate .= mercuryUpdater

venusNode = do
  sprite <- venusSprite
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 25 0 0.7
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeUpdate .= venusUpdater

earthNode = do
  sprite <- earthSprite
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 55 0 0.8
            transformAnchor .= (0.5, 0.5)
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeUpdate .= earthUpdater
    nodeChildren .= [moonNode]

moonNode = do
  sprite <- moonSprite
  return $ createNode $ do
    nodeLocalTransform .=
      createTransform
        (do transformPosition .= GL.Vector3 13 0 0.9
            transformAnchor .= (0.5, 0.5)
            transformSize .= sprite ^. spriteSize)
    nodeSprite .= Just sprite
    nodeUpdate .= moonUpdater

baseSpeed :: Double
baseSpeed = 1

mercuryUpdater :: Node -> GameState -> Node
mercuryUpdater node _ = execState modify2 node
  where
    modify2 = nodeLocalTransform . transformPolarCoordinate . polarAngle += baseSpeed / 5

venusUpdater :: Node -> GameState -> Node
venusUpdater node _ = execState modify2 node
  where
    modify2 = nodeLocalTransform . transformPolarCoordinate . polarAngle += baseSpeed / 10

earthUpdater :: Node -> GameState -> Node
earthUpdater node _ = execState modify2 node
  where
    modify2 = nodeLocalTransform . transformPolarCoordinate . polarAngle += baseSpeed / 15

moonUpdater :: Node -> GameState -> Node
moonUpdater node _ = execState modify2 node
  where
    modify2 = nodeLocalTransform . transformPolarCoordinate . polarAngle += baseSpeed * 2

sunUpdater :: Node -> GameState -> Node
sunUpdater node _ = execState modify2 node
  where
    modify2 = nodeLocalTransform . transformRotation += baseSpeed / 10

modifier :: Node -> GameState -> Node
modifier node gameState = execState (when (gameState ^. stateDragging) modify1) node
  where
    mousex = (gameState ^. stateDragStartX) + (gameState ^. stateXDelta)
    mousey = (gameState ^. stateDragStartY) + (gameState ^. stateYDelta)
    modify1 = nodeLocalTransform . transformPosition .= GL.Vector3 mousex mousey 0
