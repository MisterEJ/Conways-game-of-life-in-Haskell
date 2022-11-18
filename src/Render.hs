module Render where
import Types
import Graphics.Rendering.OpenGL

renderRect :: Rectangle -> IO ()
renderRect (Rectangle (x,y) (w,h) (r,g,b)) = preservingMatrix $ do
    color $ Color3 r g b
    translate $ Vector3 x y 0
    scale w h 1
    render 1
    where
        render :: GLfloat -> IO ()
        render w = renderPrimitive Quads $ do
            vertex $ Vertex3 w w 0.0
            vertex $ Vertex3 w (-w) 0.0
            vertex $ Vertex3 (-w) (-w) 0.0
            vertex $ Vertex3 (-w) w 0.0