module GLAssistant where

import qualified Graphics.Rendering.OpenGL as GL

init2DGL :: IO ()
init2DGL = do
  GL.blend GL.$= GL.Enabled
  GL.depthFunc GL.$= Just GL.Less
  GL.blendEquation GL.$= GL.FuncAdd
  GL.blendFunc GL.$= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.multisample GL.$= GL.Enabled
  GL.clearColor GL.$= GL.Color4 1 1 1 1
