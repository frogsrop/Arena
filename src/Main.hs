{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import           Control.Concurrent.STM
import           Control.Monad             (unless, void, when)
import           Control.Monad.RWS.Strict

import           Arena                    
import           Control.Lens
import           DatasTypesClasses
import           Demo
import           FirstScene
import           GLAssistant
import           GLFWAssistant
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Scene

main :: IO ()
main = do
  let width = 1024
      height = 576
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  withWindow width height "Arena" $ \win -> do
    setCallBacks win eventsChan --set GLFW callbacks
    init2DGL --init 2D GL with transparency support
    (fbWidth, fbHeight) <- GLFW.getFramebufferSize win
    let _demoScene = firstScene $ GL.Color4 1 1 1 1
    let env = Env {envEventsChan = eventsChan, envWindow = win, designResolution = (width, height)}
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
            , _stateMouseDown = False
            , _stateMouseClick = False
            , _stateDragging = False
            , _stateDragStartX = 0
            , _stateDragStartY = 0
            , _activeScene = _demoScene
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
