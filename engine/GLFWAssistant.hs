module GLFWAssistant where

import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad.Cont        (liftIO)
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.List                 (intercalate)
import           Data.Maybe                (catMaybes)
import           DatasTypesClasses
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW          as GLFW
import           Text.PrettyPrint          hiding ((<>))

scale :: Double
scale = 8

------------------------------------------
--------------------------------------------
-----------------CALLBACKS--------------------
------------------------------------------------
--------------------------------------------------
errorCallback :: TQueue Event -> GLFW.Error -> String -> IO ()
errorCallback tc e s = atomically $ writeTQueue tc $ EventError e s

windowPosCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowPosCallback tc win x y = atomically $ writeTQueue tc $ EventWindowPos win x y

windowSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
windowSizeCallback tc win w h = atomically $ writeTQueue tc $ EventWindowSize win w h

windowCloseCallback :: TQueue Event -> GLFW.Window -> IO ()
windowCloseCallback tc win = atomically $ writeTQueue tc $ EventWindowClose win

windowRefreshCallback :: TQueue Event -> GLFW.Window -> IO ()
windowRefreshCallback tc win = atomically $ writeTQueue tc $ EventWindowRefresh win

windowFocusCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowFocusCallback tc win fa = atomically $ writeTQueue tc $ EventWindowFocus win fa

windowIconifyCallback :: TQueue Event -> GLFW.Window -> Bool -> IO ()
windowIconifyCallback tc win ia = atomically $ writeTQueue tc $ EventWindowIconify win ia

framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int -> IO ()
framebufferSizeCallback tc win w h = atomically $ writeTQueue tc $ EventFramebufferSize win w h

mouseButtonCallback ::
     TQueue Event -> GLFW.Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
mouseButtonCallback tc win mb mba mk = atomically $ writeTQueue tc $ EventMouseButton win mb mba mk

cursorPosCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback tc win x y = atomically $ writeTQueue tc $ EventCursorPos win x y

cursorEnterCallback :: TQueue Event -> GLFW.Window -> GLFW.CursorState -> IO ()
cursorEnterCallback tc win ca = atomically $ writeTQueue tc $ EventCursorEnter win ca

scrollCallback :: TQueue Event -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback tc win x y = atomically $ writeTQueue tc $ EventScroll win x y

keyCallback :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey win k sc ka mk

charCallback :: TQueue Event -> GLFW.Window -> Char -> IO ()
charCallback tc win c = atomically $ writeTQueue tc $ EventChar win c

setCallBacks :: GLFW.Window -> TQueue Event -> IO ()
setCallBacks win eventsChan = do
  GLFW.setErrorCallback $ Just $ errorCallback eventsChan -- onError callback
  GLFW.setWindowPosCallback win $ Just $ windowPosCallback eventsChan -- onWindowPosChanged no need
  GLFW.setWindowSizeCallback win $ Just $ windowSizeCallback eventsChan -- onWindowResized no need
  GLFW.setWindowCloseCallback win $ Just $ windowCloseCallback eventsChan -- onWindowClose no need
  GLFW.setWindowRefreshCallback win $ Just $ windowRefreshCallback eventsChan -- OnWindowRefresh just for log
  GLFW.setWindowFocusCallback win $ Just $ windowFocusCallback eventsChan -- OnWindowFocus just for log
  GLFW.setWindowIconifyCallback win $ Just $ windowIconifyCallback eventsChan -- ?
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan -- ?
  GLFW.setMouseButtonCallback win $ Just $ mouseButtonCallback eventsChan -- MouseButtonCallback
  GLFW.setCursorPosCallback win $ Just $ cursorPosCallback eventsChan -- CursorPosCallback
  GLFW.setCursorEnterCallback win $ Just $ cursorEnterCallback eventsChan -- CursorEnterWindowCallback
  GLFW.setScrollCallback win $ Just $ scrollCallback eventsChan -- ScrollCallback
  GLFW.setKeyCallback win $ Just $ keyCallback eventsChan -- CommandKeyCallback
  GLFW.setCharCallback win $ Just $ charCallback eventsChan -- AnyCharCallback
  GLFW.swapInterval 1

--------------------------------------------------
------------------------------------------------
------------------CALLBACKS-------------------
--------------------------------------------
------------------------------------------
--------------------------------------------------
--------------------------------------------------
------------------------------------------
--------------------------------------------
--------------------EVENTS--------------------
------------------------------------------------
--------------------------------------------------
updateEvents :: Game_ a ()
updateEvents = do
  gameState <- get
  let mouseDown = gameState ^. stateMouseDown
  stateXScroll .= 0
  stateYScroll .= 0
  processEvents
  updatedGameState <- get
  win <- asks envWindow
  when (gameState ^. stateExit) $ liftIO $ GLFW.setWindowShouldClose win True
  when (updatedGameState ^. stateDragging) $ do
    (x, y) <- liftIO $ GLFW.getCursorPos win
    let sx = updatedGameState ^. stateDragStartX
    let sy = updatedGameState ^. stateDragStartY
    let deltax = x / scale - sx
    let deltay = y / scale - sy
    stateXDelta .= deltax
    stateYDelta .= deltay
  let mouseUp = not $ updatedGameState ^. stateMouseDown
  modify (\x -> x {_stateMouseClick = mouseDown && mouseUp})

processEvents :: Game_ a ()
processEvents = do
  tc <- asks envEventsChan
  me <- liftIO $ atomically $ tryReadTQueue tc
  case me of
    Just e -> do
      processEvent e
      processEvents
    Nothing -> return ()

processEvent :: Event -> Game_ a ()
processEvent ev =
  case ev of
    (EventError e s) -> do
      printEvent "error" [show e, show s]
      win <- asks envWindow
      liftIO $ GLFW.setWindowShouldClose win True
    (EventWindowPos _ x y) -> printEvent "window pos" [show x, show y]
    (EventWindowSize _ width height) -> printEvent "window size" [show width, show height]
    (EventWindowClose _) -> printEvent "window close" []
    (EventWindowRefresh _) -> printEvent "window refresh" []
    (EventWindowFocus _ fs) -> printEvent "window focus" [show fs]
    (EventWindowIconify _ is) -> printEvent "window iconify" [show is]
    (EventFramebufferSize _ width height) -> do
      printEvent "framebuffer size" [show width, show height]
      stateWindowWidth .= width
      stateWindowHeight .= height
      adjustWindow scale
    (EventMouseButton _ mb mbs mk) -> do
      printEvent "mouse button" [show mb, show mbs, showModifierKeys mk]
      when (mb == GLFW.MouseButton'1) $ do
        let pressed = mbs == GLFW.MouseButtonState'Pressed
        stateMouseDown .= pressed
        unless pressed $ do
          stateDragging .= False
          stateXDelta .= 0
          stateYDelta .= 0
    (EventCursorPos _ x y) -> do
      let x' = round x :: Int
          y' = round y :: Int
      stateMouseX .= x / 8
      stateMouseY .= y / 8
      --printEvent "cursor pos" [show x', show y']
      gameState <- get
      when ((gameState ^. stateMouseDown) && not (gameState ^. stateDragging)) $ do
        stateDragging .= True
        stateDragStartX .= x / 8
        stateDragStartY .= y / 8
    (EventCursorEnter _ cs) -> printEvent "cursor enter" [show cs]
    (EventScroll _ x y) -> do
      let x' = x
          y' = y
      stateXScroll .= x'
      stateYScroll .= y'
      printEvent "scroll" [show x', show y']
    (EventKey win k scancode ks mk) -> do
      printEvent "key" [show k, show scancode, show ks, showModifierKeys mk]
      when (k == GLFW.Key'Down || k == GLFW.Key'Up || k == GLFW.Key'Left || k == GLFW.Key'Right) $ do
        (keydx, keydy) <- liftIO $ getCursorKeyDirections win
        liftIO $ putStrLn ("X direction = " ++ show keydx ++ " Y direction = " ++ show keydy)
      when (ks == GLFW.KeyState'Pressed) $
        -- Esc: exit
       do
        when (k == GLFW.Key'Escape) $ liftIO $ GLFW.setWindowShouldClose win True
        -- ?: modifier test
        when (k == GLFW.Key'Slash && GLFW.modifierKeysShift mk) $ liftIO $ putStrLn "Nice try"
        -- i: print GLFW information
        when (k == GLFW.Key'I) $ liftIO $ printInformation win
    (EventChar _ c) -> printEvent "char" [show c]

printEvent :: String -> [String] -> Game_ a ()
printEvent cbname fields = liftIO $ putStrLn $ cbname ++ ": " ++ unwords fields

--------------------------------------------------
------------------------------------------------
--------------------EVENTS--------------------
--------------------------------------------
------------------------------------------
--------------------------------------------------
--------------------------------------------------
------------------------------------------
--------------------------------------------
--------------------KEYS----------------------
------------------------------------------------
--------------------------------------------------
getCursorKeyDirections :: GLFW.Window -> IO (Double, Double)
getCursorKeyDirections win = do
  x0 <- isPress <$> GLFW.getKey win GLFW.Key'Left
  x1 <- isPress <$> GLFW.getKey win GLFW.Key'Right
  y0 <- isPress <$> GLFW.getKey win GLFW.Key'Down
  y1 <- isPress <$> GLFW.getKey win GLFW.Key'Up
  let x0n =
        if x0
          then (-1)
          else 0
      x1n =
        if x1
          then 1
          else 0
      y0n =
        if y0
          then (-1)
          else 0
      y1n =
        if y1
          then 1
          else 0
  return (x0n + x1n, y0n + y1n)

isPress :: GLFW.KeyState -> Bool
isPress GLFW.KeyState'Pressed   = True
isPress GLFW.KeyState'Repeating = True
isPress _                       = False

showModifierKeys :: GLFW.ModifierKeys -> String
showModifierKeys mk = "[mod keys: " ++ keys ++ "]"
  where
    keys =
      if null xs
        then "none"
        else unwords xs
    xs = catMaybes ys
    ys =
      [ if GLFW.modifierKeysShift mk
          then Just "shift"
          else Nothing
      , if GLFW.modifierKeysControl mk
          then Just "control"
          else Nothing
      , if GLFW.modifierKeysAlt mk
          then Just "alt"
          else Nothing
      , if GLFW.modifierKeysSuper mk
          then Just "super"
          else Nothing
      ]

--------------------------------------------------
------------------------------------------------
--------------------KEYS----------------------
--------------------------------------------
------------------------------------------
--------------------------------------------------
--------------------------------------------------
------------------------------------------
--------------------------------------------
--------------------WINDOW--------------------
------------------------------------------------
--------------------------------------------------
adjustWindow :: Double -> Game_ a ()
adjustWindow globalScale = do
  gameState <- get
  let width = gameState ^. stateWindowWidth
      height = gameState ^. stateWindowHeight
  let size = GL.Size (fromIntegral width) (fromIntegral height)
      dwidth = fromIntegral width :: Double
      dheight = fromIntegral height :: Double
      pos = GL.Position 0 0
  liftIO $ do
    GL.viewport GL.$= (pos, size)
    GL.matrixMode GL.$= GL.Projection
    GL.loadIdentity --normalize matrix
    GL.ortho2D 0 (dwidth / globalScale) (dheight / globalScale) 0

--------------------------------------------------
------------------------------------------------
--------------------WINDOW--------------------
--------------------------------------------
------------------------------------------
--------------------------------------------------
--------------------------------------------------
------------------------------------------
--------------------------------------------
--------------------OTHER---------------------
------------------------------------------------
--------------------------------------------------
printInformation :: GLFW.Window -> IO ()
printInformation win = do
  version <- GLFW.getVersion
  versionString <- GLFW.getVersionString
  monitorInfos <- runMaybeT getMonitorInfos
  clientAPI <- GLFW.getWindowClientAPI win
  cv0 <- GLFW.getWindowContextVersionMajor win
  cv1 <- GLFW.getWindowContextVersionMinor win
  cv2 <- GLFW.getWindowContextVersionRevision win
  robustness <- GLFW.getWindowContextRobustness win
  forwardCompat <- GLFW.getWindowOpenGLForwardCompat win
  debug <- GLFW.getWindowOpenGLDebugContext win
  profile <- GLFW.getWindowOpenGLProfile win
  putStrLn $
    render $
    nest
      4
      (text "------------------------------------------------------------" $+$ text "GLFW C library:" $+$
       nest
         4
         (text "Version:" <+> renderVersion version $+$ text "Version string:" <+> renderVersionString versionString) $+$
       text "Monitors:" $+$
       nest 4 (renderMonitorInfos monitorInfos) $+$
       text "OpenGL context:" $+$
       nest
         4
         (text "Client API:" <+>
          renderClientAPI clientAPI $+$ text "Version:" <+>
          renderContextVersion cv0 cv1 cv2 $+$ text "Robustness:" <+>
          renderContextRobustness robustness $+$ text "Forward compatibility:" <+>
          renderForwardCompat forwardCompat $+$ text "Debug:" <+>
          renderDebug debug $+$ text "Profile:" <+> renderProfile profile) $+$
       text "------------------------------------------------------------")
  where
    renderVersion (GLFW.Version v0 v1 v2) = text $ intercalate "." $ map show [v0, v1, v2]
    renderVersionString = text . show
    renderMonitorInfos = maybe (text "(error)") (vcat . map renderMonitorInfo)
    renderMonitorInfo (name, (x, y), (w, h), vms) =
      text (show name) $+$ nest 4 (location <+> size $+$ fsep (map renderVideoMode vms))
      where
        location = int x <> text "," <> int y
        size = int w <> text "x" <> int h <> text "mm"
    renderVideoMode (GLFW.VideoMode w h r g b rr) = brackets $ res <+> rgb <+> hz
      where
        res = int w <> text "x" <> int h
        rgb = int r <> text "x" <> int g <> text "x" <> int b
        hz = int rr <> text "Hz"
    renderContextVersion v0 v1 v2 = hcat [int v0, text ".", int v1, text ".", int v2]
    renderClientAPI = text . show
    renderContextRobustness = text . show
    renderForwardCompat = text . show
    renderDebug = text . show
    renderProfile = text . show

getMonitorInfos :: MaybeT IO [MonitorInfo]
getMonitorInfos = getMonitors >>= mapM getMonitorInfo
  where
    getMonitors :: MaybeT IO [GLFW.Monitor]
    getMonitors = MaybeT GLFW.getMonitors
    getMonitorInfo :: GLFW.Monitor -> MaybeT IO MonitorInfo
    getMonitorInfo mon = do
      name <- getMonitorName mon
      vms <- getVideoModes mon
      MaybeT $ do
        pos <- liftIO $ GLFW.getMonitorPos mon
        size <- liftIO $ GLFW.getMonitorPhysicalSize mon
        return $ Just (name, pos, size, vms)
    getMonitorName :: GLFW.Monitor -> MaybeT IO String
    getMonitorName mon = MaybeT $ GLFW.getMonitorName mon
    getVideoModes :: GLFW.Monitor -> MaybeT IO [GLFW.VideoMode]
    getVideoModes mon = MaybeT $ GLFW.getVideoModes mon
--------------------------------------------------
------------------------------------------------
--------------------OTHER--------------------
--------------------------------------------
------------------------------------------
