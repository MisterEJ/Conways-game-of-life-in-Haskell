import Graphics.UI.GLUT

import Bindings ( keyboardMouse, reshape, display, idle, display )
import Data.IORef (newIORef)
import Game
import Types

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Conways game of life"
  windowSize $= Size 1024 768
  reshapeCallback $= Just reshape

  state <- newIORef ready
  isRunning <- newIORef False

  keyboardMouseCallback $= Just (keyboardMouse isRunning state)

  idleCallback $= Just (idle state isRunning)
  displayCallback $= display state

  mainLoop