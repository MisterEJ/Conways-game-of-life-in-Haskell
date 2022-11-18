module Bindings (display, reshape, keyboardMouse, idle) where

import Graphics.UI.GLUT
import Display ( display, idle )

reshape :: ReshapeCallback
reshape s = do 
  viewport $= (Position 0 0, s)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()