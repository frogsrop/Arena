module Node where

import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Writer      (MonadWriter)
import           Data.Function             ((&))
import           DatasTypesClasses
import           GHC.IO.Unsafe             (unsafePerformIO)
import qualified Graphics.Rendering.OpenGL as GL
import           Transform

collectDisplayListsWithPosition :: Node_ a -> IO [(GL.DisplayList, Transform)]
collectDisplayListsWithPosition node = do
  currentNodeChildren <- sequence $ node & _nodeChildren
  liftM2 (++) (return spritesDisplayLists) (collector currentNodeChildren)
  where
    spritesDisplayLists =
      case node & _nodeSprite of
        Just sprite -> [(sprite & _spriteTexture, node ^. nodeGlobalTransform)]
        Nothing -> []
    collector :: [Node_ a] -> IO [(GL.DisplayList, Transform)]
    collector nodes = concat <$> mapM collectDisplayListsWithPosition nodes


setNodeLocalTransform :: Transform -> Node_ a -> Node_ a
setNodeLocalTransform transform' = execState (nodeLocalTransform .= transform')

setNodeParentTransform :: Transform -> Node_ a -> Node_ a
setNodeParentTransform transform' = execState (nodeParentTransform .= transform')

instance Updatable Node_ where
  update node gameState =
    let (updnode, function) =
          ((node ^. nodeUpdate) (updateNodeGlobalCoordinate node) gameState, (node ^. nodeGameStateUpdate) node)
     in (updnode, [function])

updateChildren :: GameState_ a -> Node_ a -> IO (Node_ a)
updateChildren gameState node = do
  children_ <- sequence $ node ^. nodeChildren
  let updatedPolarCoordinates = map (execState (nodeParentTransform .= node ^. nodeGlobalTransform)) children_
      (updatedChildren, _) =
        foldr
          ((\(node1, functions1) (node2, functions2) -> (node1 : node2, functions1 ++ functions2)) .
           (`update` gameState))
          ([], [])
          updatedPolarCoordinates
  return $ node {_nodeChildren = map return updatedChildren}

updateNodeGlobalCoordinate :: Node_ a -> Node_ a
updateNodeGlobalCoordinate node = node {_nodeGlobalTransform = getGlobalTransform node}

getGlobalTransform :: Node_ a -> Transform
getGlobalTransform parent = execState (fullUpdate parent) zeroTransform
  where
    fullUpdate :: Node_ a -> State Transform ()
    fullUpdate _parent = do
      let parentTransform = _parent ^. nodeParentTransform
          localTransform = _parent ^. nodeLocalTransform
      transformPosition .= localTransform ^. transformPosition
      transformPolarCoordinate .= getRealPolarCoordinates parentTransform localTransform
      transformRotation .= parentTransform ^. transformRotation + localTransform ^. transformRotation
      let (pxs, pys) = parentTransform ^. transformScale
      let (cxs, cys) = localTransform ^. transformScale
      --transformScale .= (pxs * cxs, pys * cys)
      transformScale .= (cxs, cys)
      transformAnchor .= localTransform ^. transformAnchor
      transformSize .= localTransform ^. transformSize

getRealPolarCoordinates :: Transform -> Transform -> PolarCoordinate
getRealPolarCoordinates parent child =
  let parentCoordinate = parent ^. transformPolarCoordinate
      childCoordinate = child ^. transformPolarCoordinate
      childR = childCoordinate ^. polarR
      childAngle = childCoordinate ^. polarAngle
      parentR = parentCoordinate ^. polarR
      parentAngle = parentCoordinate ^. polarAngle
      parentRotation = parent ^. transformRotation
      (realR, realAngle) = evaluateRealPolarCoordinates parentR childR parentRotation childAngle parentAngle
   in PolarCoordinate {_polarR = realR, _polarAngle = realAngle}

evaluateRealPolarCoordinates :: Double -> Double -> Double -> Double -> Double -> (Double, Double)
evaluateRealPolarCoordinates r1 rd ap ad a1 =
  let rad_ap = ap * pi / 180
      rad_ad = ad * pi / 180
      rad_a1 = -a1 * pi / 180
      deltha = pi - rad_a1 - rad_ad - rad_ap
      r2 = sqrt (r1 * r1 + rd * rd - 2 * r1 * rd * cos deltha)
      gammas = asin (rd * sin deltha / r2)
      gammac
        | r1 == 0 = rad_ad + rad_ap
        | r2 == 0 = 0
        | otherwise = acos ((r1 * r1 + r2 * r2 - rd * rd) / (2 * r1 * r2))
      (gs, gc) = (gammas, gammac)
      gamma = gc * signum gs
      a2 = (rad_a1 - gamma) * 180 / pi
   in if r2 == 0
        then (0, 0)
        else (r2, -a2)

nodeSprites :: Lens' (Node_ a) (Maybe Sprite)
nodeSprites = lens _nodeSprite (\sprites newSprites -> sprites {_nodeSprite = newSprites})

defaultNode :: Node_ a
defaultNode =
  Node_
    { _nodeParentTransform = zeroTransform
    , _nodeChildren = []
    , _nodeLocalTransform = zeroTransform
    , _nodeSprite = Nothing
    , _nodeUpdate = const
    , _nodeGameStateUpdate = const id
    , _nodeGlobalTransform = zeroTransform
    , _nodeCollider = Nothing
    }

createNode :: State (Node_ a) () -> Node_ a
createNode st = execState st defaultNode
