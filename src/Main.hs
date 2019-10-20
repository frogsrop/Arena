{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Control.Concurrent.STM
import           Control.Monad             (unless, void, when)
import           Control.Monad.RWS.Strict

import           Arena
import           CaveScene
import           Control.Lens
import           DatasTypesClasses
import           Demo
import           FirstScene
import           GLAssistant
import           GLFWAssistant
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           InventoryScene
import           Mobs
import           Scene
import           Sprite
import           System.Random

main :: IO ()
main = do
  let width = 1024
      height = 576
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  withWindow width height "Arena" $ \win -> do
    setCallBacks win eventsChan --set GLFW callbacks
    init2DGL --init 2D GL with transparency support
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
    _firstScene <- firstScene $ GL.Color4 0.3 0.3 0.3 1
    _demoScene <- demoScene $ GL.Color4 0.05 0.05 0.05 1
    _caveScene <- caveScene $ GL.Color4 0 0 0 1
    _inventoryScene <- inventoryScene $ GL.Color4 0 0 0 1
    helmet1 <- createSpriteWithPath "assets/sprites/png_pictures/inventory/helmet1.png"
    helmet2 <- createSpriteWithPath "assets/sprites/png_pictures/inventory/helmet2.png"
    let env = Env {envEventsChan = eventsChan, envWindow = win, designResolution = (width, height)}
    mobs <- sequence [slime]
    rnd <- randomIO
    let gameState =
          GameState_
            { _stateWindowWidth = fbWidth
            , _stateWindowHeight = fbHeight
            , _stateXScroll = 0
            , _stateYScroll = 0
            , _stateXDelta = 0
            , _stateYDelta = 0
            , _stateMouseX = 0
            , _stateMouseY = 0
            , _stateExit = False
            , _stateMouseDown = False
            , _stateMouseClick = False
            , _stateDragging = False
            , _stateDragStartX = 0
            , _stateDragStartY = 0
            , _activeScene = _inventoryScene
            , _gameInfo =
                GameInfo
                  { _gameInfoMobsList = mobs
                  , _gameInfoCurrentFight = Nothing
                  , _gameInfoPlayerInfo =
                      PlayerInfo
                        { _playerInfoAtackCooldown = 15
                        , _playerInfoAtk = 1
                        , _playerInfoDef = 1
                        , _playerInfoHP = 10
                        , _playerInfoInventory = [helmet1, helmet2, helmet2, helmet2, helmet2, helmet2, helmet2, helmet2]
                        }
                  }
            , _randomGenerator = mkStdGen rnd
            , _scenes = [_firstScene, _caveScene, _demoScene, _inventoryScene]
            }
    runArena env gameState
  putStrLn "ended!"

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  GLFW.setErrorCallback $ Just simpleErrorCallback
  r <- GLFW.init
  when r $ do
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      (Just win) -> do
        GLFW.makeContextCurrent m
        f win
        GLFW.setErrorCallback $ Just simpleErrorCallback
        GLFW.destroyWindow win
      Nothing -> return ()
    GLFW.terminate
  where
    simpleErrorCallback e s = putStrLn $ unwords [show e, show s]

runArena :: Env -> GameState -> IO ()
runArena env gameState = void $ evalRWST (adjustWindow scale >> runGameCycle) env gameState

runGameCycle :: Game ()
runGameCycle = do
  win <- asks envWindow
  liftIO GLFW.pollEvents
  updateEvents -- gameState updates with events
  gameState <- get
  updateAll
  drawAll win
  q <- liftIO $ GLFW.windowShouldClose win
  unless q runGameCycle

drawAll :: GLFW.Window -> Game ()
drawAll win = do
  gameState <- get
  liftIO $ GL.clear [GL.ColorBuffer, GL.DepthBuffer, GL.StencilBuffer]
  drawScene (gameState & _activeScene)
  liftIO $ do
    GLFW.swapBuffers win
    GL.flush

updateAll :: Game ()
updateAll = do
  gameState <- get
  updateScene (gameState & _activeScene)
