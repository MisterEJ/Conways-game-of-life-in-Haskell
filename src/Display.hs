module Display(display, idle, Rectangle(..)) where

import Graphics.UI.GLUT
import Types
import Game(process, render, State(..))
import Data.IORef
import Render (renderRect)
import Control.Monad

display :: IORef State -> DisplayCallback
display state = do
    clear [ ColorBuffer ]
    matrixMode $= Projection
    loadIdentity
    ortho 0 1024 768 0 0 100
    matrixMode $= Modelview 0

    s <- get state
    preservingMatrix $ render s

    swapBuffers

idle :: IORef State -> IORef Bool -> IdleCallback
idle state isRunning = do
    run <- get isRunning
    when run $ state $~! process
    postRedisplay Nothing