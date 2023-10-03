{- |
Module      : Tetris
Description : The Tetris game (main module)
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Theodor Köhler, Daniel Rising, Ludvig Ingolfson
Lab group   : 31
-}

module Main where

import Data.Maybe (isJust)
import Data.List (partition)

import ConsoleGUI
-- import ThreepennyGUI  -- either use ConsoleGUI or ThreepennyGUI

import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together
main :: IO ()
main = runGame tetrisGame

tetrisGame :: Game Tetris
tetrisGame = Game
  { startGame     = startTetris
  , stepGame      = stepTetris
  , drawGame      = drawTetris
  , gameInfo      = defaultGameInfo prop_Tetris
  , tickDelay     = defaultDelay
  , gameInvariant = prop_Tetris
  }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation

type Piece = (Pos, Shape)
type Pos   = (Int, Int)

-- | The state of the game consists of three parts:
data Tetris = Tetris
  { piece  :: Piece    -- ^ The position and shape of the falling piece
  , well   :: Shape    -- ^ The well (the playing field), where the falling pieces pile up
  , shapes :: [Shape]  -- ^ An infinite supply of random shapes
  }

-- | The size of the well
wellWidth, wellHeight :: Int
wellWidth  = 10
wellHeight = 20

wellSize :: (Int, Int)
wellSize   = (wellHeight, wellWidth)

-- | Starting position for falling pieces
startPosition :: Pos
startPosition = (0, wellWidth `div` 2 - 1)

-- | Pos addition
add :: Pos -> Pos -> Pos
(h1, w1) `add` (h2, w2) = (h1 + h2, w1 + w2)

-- | Move the falling piece into position
place :: (Pos, Shape) -> Shape
place (v, s) = shiftShape v s

-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris t = prop_piece && prop_well && prop_collision where
  prop_piece      = prop_Shape $ snd $ piece t
  prop_well       = shapeSize (well t) == wellSize
  prop_collision  = not $ collision t
  
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = addWallsTopBottom $ addWallsSides s

addWallsTopBottom :: Shape -> Shape
addWallsTopBottom (Shape rs) = Shape ([r] ++ rs ++ [r]) where
  r = blackRow (length (head rs))
  blackRow n = replicate n $ Just Black

addWallsSides :: Shape -> Shape
addWallsSides (Shape rs) = Shape [[Just Black] ++ r ++ [Just Black] | r <- rs]

-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v, p) w _) = addWalls $ combine w $ shiftShape v p

-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition, piece) well supply
 where
  well          = emptyShape wellSize
  piece:supply  = [allShapes !! (getIndex r) | r <- rs]
  getIndex 1.0 = length allShapes - 1 -- Extremely unlikely special case
  getIndex d    = floor $ d * (fromIntegral $ length allShapes)

-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int, Tetris)
stepTetris Tick t       = tick t
stepTetris MoveDown t   = tick t
stepTetris MoveLeft t   = Just (0, movePiece (-1) t)
stepTetris MoveRight t  = Just (0, movePiece 1 t)
stepTetris Rotate t     = Just (0, rotatePiece t)

move :: (Int, Int) -> Tetris -> Tetris
move p' (Tetris (p, s) w ss) = Tetris (p `add` p', s) w ss

tick :: Tetris -> Maybe (Int, Tetris)
tick t  | collision newState = dropNewPiece t
        | otherwise = Just (0, newState)
  where
    newState = move (1, 0) t

collision :: Tetris -> Bool
collision (Tetris ((y, x), s) w _) 
  | x < 0                               = True
  | snd (shapeSize s) + x > wellWidth   = True
  | fst (shapeSize s) + y > wellHeight  = True
  | overlaps w (place ((y, x), s))      = True
  | otherwise                           = False

movePiece :: Int -> Tetris -> Tetris
movePiece x t | collision newState  = t
              | otherwise           = newState
 where
  newState = move (0, x) t

rotate :: Tetris -> Tetris
rotate (Tetris (p, s) w ss) = Tetris (p, rotateShape s) w ss

adjust :: Tetris -> Tetris
adjust (Tetris ((y, x), s) w ss)
  | x < 0                             = adjustDir 1
  | snd (shapeSize s) + x > wellWidth = adjustDir (-1)
  | otherwise                         = Tetris ((y, x), s) w ss
  where
    adjustDir x'
      | collision t = adjust t
      | otherwise   = t
      where
        t = Tetris ((y, x + x'), s) w ss

rotatePiece :: Tetris -> Tetris
rotatePiece t | collision newState  = t
              | otherwise           = newState
  where
    newState = adjust (rotate t)

dropNewPiece :: Tetris -> Maybe (Int, Tetris)
dropNewPiece (Tetris p w (s:ss))
  | overlaps newWell (place newPiece) = Nothing
  | otherwise                         = Just (n, newState)
  where
    newState      = Tetris newPiece newWell ss
    newPiece      = (startPosition, s)
    (n, newWell)  = clearLines $ combine (place p) w

clearLines :: Shape -> (Int, Shape)
clearLines s = (score, Shape (newRows ++ remainder)) where
  newRows                 = rows $ emptyShape (score, wellWidth)
  score                   = length completed
  (completed, remainder)  = partition isComplete $ rows s
  isComplete row          = and [isJust sq | sq <- row]