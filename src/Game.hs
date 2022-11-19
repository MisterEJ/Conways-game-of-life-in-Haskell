{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Game(process, State(..), ready, render, cellCount, generateCells, checkCell, changeState) where

import Types
import Render (renderRect)
import Control.Monad

newtype State = State [Rectangle] deriving Show

cellSize :: Float
cellSize = 16

cellCount :: (Int, Int)
cellCount = (floor (1024 / cellSize), round (768 / cellSize))

generateCells :: [Rectangle]
generateCells = [Rectangle (cellSize * fromIntegral x, fromIntegral y * cellSize) (cellSize, cellSize) (0, 0, 0) | x <- [0..fst cellCount - 1],  y <- [0..snd cellCount - 1]]

ready :: State
ready = State generateCells

process :: State -> State
process (State s) = State $ process' s s where
    process' :: [Rectangle] -> [Rectangle] -> [Rectangle]
    process' [] _ = []
    process' (x:xs) l = checkCell x l  : process' xs l


checkCell :: Rectangle -> [Rectangle] -> Rectangle
checkCell r [] = r
checkCell (Rectangle (x,y) s' c') c = do
    let sc = countCells (checkList (x,y)) c
    if sc == 3 then Rectangle (x,y) s' (1,1,1) else if sc < 2 || sc > 3 then Rectangle (x,y) s' (0,0,0) else Rectangle (x,y) s' c' 
    where
        countCells :: [(Float, Float)] -> [Rectangle] -> Int
        countCells [] _ = 0
        countCells (x:xs) c = countCell x c + countCells xs c

        countCell :: (Float, Float) -> [Rectangle] -> Int
        countCell _ [] = 0
        countCell (x, y) ((Rectangle (x1, y1) _ (c, _, _)):xs) = if (x == x1) && (y == y1) && c == 1 then 1 else countCell (x,y) xs

        checkList :: (Float, Float) -> [(Float, Float)]
        checkList (x,y) = [(x + cellSize, y + cellSize), (x + cellSize, y), (x + cellSize, y - cellSize), (x, y - cellSize), (x - cellSize, y - cellSize), (x - cellSize, y), (x - cellSize, y + cellSize), (x, y + cellSize)]

changeState :: State -> (Float, Float) -> State
changeState (State l) rect = State $ changeState' l rect
    where
        changeState' :: [Rectangle] -> (Float, Float) -> [Rectangle]
        changeState' [] _ = []
        changeState' (rect0@(Rectangle (x,y) s (c, _, _)):xs) rect'@(x1,y1) = do
            let x3 = fromIntegral $ floor (x1 / 16) * 16 + 16
            let y3 = fromIntegral $ floor (y1 / 16) * 16 + 16
            if x == x3 && y == y3
                then if c == 1
                    then Rectangle (x,y) s (0, 0, 0) : changeState' xs rect'
                    else Rectangle (x,y) s (1, 1, 1) : changeState' xs rect'
                else rect0 : changeState' xs rect'

render :: State -> IO ()
render (State cells') = forM_ cells' renderRect