module Bindings (display, reshape, keyboardMouse, idle) where

import Graphics.UI.GLUT
import Display ( display, idle )
import Data.IORef
import Types
import Game

reshape :: ReshapeCallback
reshape s = do 
  viewport $= (Position 0 0, s)

keyboardMouse :: IORef Bool -> IORef State -> KeyboardMouseCallback
keyboardMouse run _ (Char ' ') Down _ _ = do
  r <- get run
  run $= not r
  postRedisplay Nothing

keyboardMouse _ state (MouseButton LeftButton) Down _ (Position x y) = do
  s <- get state
  state $= changeState s (fromIntegral x, fromIntegral y)
  postRedisplay Nothing

keyboardMouse _ _ _ _ _ _ = return ()