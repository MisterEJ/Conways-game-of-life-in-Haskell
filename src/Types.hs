module Types where
import Graphics.Rendering.OpenGL

data Rectangle = Rectangle (Float, Float) (Float, Float) (Float, Float, Float)
    deriving Show