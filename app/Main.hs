import Graphics.UI.GLUT

import Bindings ( keyboardMouse, reshape, display, idle, display )
import Data.IORef (newIORef)
import Game

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Conways game of life"
  windowSize $= Size 1024 768
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse

  state <- newIORef ready

  idleCallback $= Just (idle state)
  displayCallback $= display state

  mainLoop