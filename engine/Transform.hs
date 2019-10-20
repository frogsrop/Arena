module Transform where

import           Control.Lens
import           Control.Monad.State
import           DatasTypesClasses
import qualified Graphics.Rendering.OpenGL as GL

zeroTransform :: Transform
zeroTransform =
  Transform
    { _transformPosition' = GL.Vector3 0 0 0
    , _transformPolarCoordinate' = PolarCoordinate {_polarR = 0, _polarAngle = 0}
    , _transformRotation = 0
    , _transformScale = (1, 1)
    , _transformAnchor = (0, 0)
    , _transformSize = GL.Size 0 0
    }

transformWithPosition :: GL.Vector3 Double -> Transform
transformWithPosition position = set transformPosition position zeroTransform

createTransform :: State Transform () -> Transform
createTransform st = execState st zeroTransform

getZOrder :: Transform -> Double
getZOrder _transform =
  case view transformPosition _transform of
    GL.Vector3 _ _ z -> z
