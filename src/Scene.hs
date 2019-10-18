{-# LANGUAGE TypeApplications #-}

module Scene where

import           Control.Lens
import           Control.Monad.RWS.Strict
import           Data.Function             ((&))
import           Data.Sort
import           DatasTypesClasses
import qualified Graphics.Rendering.OpenGL as GL
import           Node
import           Transform

updateScene :: IO (Scene_ a) -> Game_ a ()
updateScene ioScene = do
  gameState <- get
  scene <- liftIO ioScene
  let nodes = scene & _sceneChildren
  (updatedChildren, gameStateUpdateFunctions) <- liftIO $ updateHelper nodes gameState
  put $ set activeScene (return scene {_sceneChildren = updatedChildren}) gameState
  updGameState <- get
  put $ foldl (&) updGameState gameStateUpdateFunctions
  where
    updateHelper :: [Node_ a] -> GameState_ a -> IO ([Node_ a], [(GameState_ a) -> (GameState_ a)])
    updateHelper [] _ = return ([], [])
    updateHelper (x:xs) gameState = mergeNodeWithTail
      where
        (nodeSelfUpdated, functionsSelfUpdated) = update x gameState
        updNodes = do
          nodeWithUpdatedPosition <- updateChildren gameState nodeSelfUpdated
          updatedNodeChildren <- sequence $ nodeWithUpdatedPosition & _nodeChildren
          (updatedChildren, updatedFunctions) <- updateHelper updatedNodeChildren gameState
          let finishedNode = nodeWithUpdatedPosition {_nodeChildren = map return updatedChildren}
          return (finishedNode, functionsSelfUpdated ++ updatedFunctions)
        mergeNodeWithTail = do
          (node, functions) <- updNodes
          (xsnodes, xsfunctions) <- updateHelper xs gameState
          return (node : xsnodes, functions ++ xsfunctions)

drawScene :: IO (Scene_ a) -> Game_ a ()
drawScene ioScene = do
  scene <- liftIO ioScene
  fromIOList <- liftIO $ concat <$> mapM collectDisplayListsWithPosition (scene & _sceneChildren)
  let displayLists = fromIOList
  let sortedLists = sortBy zOrder displayLists
  drawHelper sortedLists
  where
    drawHelper :: [(GL.DisplayList, Transform)] -> Game_ a ()
    drawHelper [] = return ()
    drawHelper ((texture, _transform):xs) = do
      let GL.Vector3 _ _ z = view transformPosition _transform
      let rotation = _transform & _transformRotation
      let (sx, sy) = _transform & _transformScale
      let (ax, ay) = _transform & _transformAnchor
      let (w, h) =
            let GL.Size w h = _transform & _transformSize
             in (fromIntegral w, fromIntegral h)
      let PolarCoordinate r angle = _transform ^. transformPolarCoordinate
      drawHelper xs
      liftIO $
        GL.preservingMatrix $ do
          GL.rotate angle $ GL.Vector3 @Double 0 0 1
          GL.translate $ GL.Vector3 @Double r 0 z
          GL.rotate (rotation - angle) $ GL.Vector3 @Double 0 0 1
          GL.scale @Double sx sy 1
          GL.translate $ GL.Vector3 @Double (-w * ax) (-h * ay) 0
          GL.callList texture
    zOrder :: (GL.DisplayList, Transform) -> (GL.DisplayList, Transform) -> Ordering
    zOrder (_, transform1) (_, transform2)
      | getZOrder transform1 < getZOrder transform2 = GT
      | getZOrder transform1 == getZOrder transform2 = EQ
      | otherwise = LT
