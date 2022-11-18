{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Game(process, State(..), ready, render) where

import Types
import Render (renderRect)
import Control.Monad

newtype State = State [Rectangle]

cellSize :: Float
cellSize = 10

cellCount :: (Float, Float)
cellCount = (1024 / cellSize, 768 / cellSize)

generateCells :: [Rectangle]
generateCells = [Rectangle (x * cellSize, y * cellSize) (cellSize, cellSize) (cos x, sin y, cos x) | x <- [0..fst cellCount],  y <- [0..snd cellCount]]

ready :: State
ready = State generateCells

process :: State -> State
process (State s) = State $ process' s where
    process' :: [Rectangle] -> [Rectangle]
    process' [] = []
    process' ((Rectangle (x,y) s' (r,g,b)):xs) = Rectangle (x+1,y) s' (sin x, sin y, sin (x+y)) : process' xs

render :: State -> IO ()
render (State cells') = forM_ cells' renderRect