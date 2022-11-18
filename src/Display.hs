module Display(display, idle, Rectangle(..)) where

import Graphics.UI.GLUT
import Types
import Game(process, render, State(..))
import Data.IORef

display :: IORef State -> DisplayCallback
display state = do
    clear [ ColorBuffer ]

    matrixMode $= Projection
    loadIdentity
    ortho 0 1024 0 768 0 100
    matrixMode $= Modelview 0

    s <- get state
    preservingMatrix $ render s

    swapBuffers

idle :: IORef State -> IdleCallback
idle state = do
    state $~! process
    postRedisplay Nothing