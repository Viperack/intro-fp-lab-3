{- |
Module      : Shapes
Description : Types and functions for shapes. The list of all tetris pieces.
Copyright   : (c) TDA555/DIT441, Introduction to Functional Programming
License     : BSD
Maintainer  : alexg@chalmers.se
Stability   : experimental

Authors     : Theodor Köhler, Daniel Rising, Ludvig Ingolfson
Lab group   : 31
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Shapes where

import Data.List (transpose)
import Data.Maybe (isNothing)
import Test.QuickCheck
import Control.Applicative (Alternative(empty))
import GHC.CmmToAsm.X86.Regs (r10, r12)

-- * Shapes

data Colour = Black | Red | Green | Yellow | Blue | Purple | Cyan | Grey
  deriving (Eq, Bounded, Enum, Show)

type Square = Maybe Colour

-- | A geometric shape is represented as a list of lists of squares. Each square
-- can be empty or filled with a block of a specific colour.
type Row   = [Square]
data Shape = Shape { rows :: [Row] } deriving Eq

-- * Showing shapes
showShape :: Shape -> String
showShape s = unlines [showRow r | r <- rows s]
 where
  showRow r = [showSquare s | s <- r]

  showSquare Nothing      = '.'
  showSquare (Just Black) = '#' -- can change to '█' on linux/mac
  showSquare (Just Grey)  = 'g' -- can change to '▓'
  showSquare (Just c)     = head (show c)

instance Show Shape where
  show = showShape
  showList ss r = unlines (map show ss) ++ r

-- * The shapes used in the Tetris game

-- | All 7 tetrominoes (all combinations of 4 connected blocks),
-- see <https://en.wikipedia.org/wiki/Tetromino>
allShapes :: [Shape]
allShapes = [Shape (makeSquares s) | s <- shapes]
 where
   makeSquares = map (map colour)
   colour c    = lookup c [ ('I', Red),  ('J', Grey),  ('T', Blue), ('O', Yellow)
                          , ('Z', Cyan), ('L', Green), ('S', Purple) ]
   shapes = [["I",
              "I",
              "I",
              "I"],
             [" J",
              " J",
              "JJ"],
             [" T",
              "TT",
              " T"],
             ["OO",
              "OO"],
             [" Z",
              "ZZ",
              "Z "],
             ["LL",
              " L",
              " L"],
             ["S ",
              "SS",
              " S"]]

-- * Some simple functions

-- ** A1
emptyShape :: (Int, Int) -> Shape
emptyShape (n1, n2) = Shape (replicate n1 (emptyRow n2))

emptyRow :: Int -> Row
emptyRow n = replicate n Nothing

-- ** A2

-- | The size (width and height) of a shape
shapeSize :: Shape -> (Int, Int)
shapeSize (Shape rs) = (length rs, length (head rs))

-- ** A3

-- | Count how many non-empty squares a shape contains
blockCount :: Shape -> Int
blockCount s = length squares - countElem Nothing squares
 where
  squares = concat (rows s)

countElem :: Eq a => a -> [a] -> Int
countElem _ [] = 0
countElem x (e:es) | x == e    = countElem x es + 1
                   | otherwise = countElem x es

-- Quicker solution for all tetrominoes
-- blockCount :: Int
-- blockCount = 4

-- * The Shape invariant

-- ** A4
-- | Shape invariant (shapes have at least one row, at least one column,
-- and are rectangular)
prop_Shape :: Shape -> Bool
prop_Shape rs = prop_Shape_size rs && prop_Shape_rect rs

-- Checks that there is at least 1 row and 1 column
prop_Shape_size :: Shape -> Bool
prop_Shape_size (Shape rs) = not (null rs) && not (null (head rs)) 

-- Checks that all the rows are of the same length
prop_Shape_rect :: Shape -> Bool
prop_Shape_rect (Shape (r1:r2:rs)) = length r1 == length r2
 && prop_Shape_rect (Shape (r2:rs))
prop_Shape_rect _ = True

-- * Test data generators

-- ** A5
-- | A random generator for colours
genColour :: Gen Colour
genColour = elements [Black, Red, Green, Yellow, Blue, Purple, Cyan, Grey]

instance Arbitrary Colour where
  arbitrary = genColour

-- ** A6
-- | A random generator for shapes
genShape :: Gen Shape
genShape = elements allShapes

instance Arbitrary Shape where
  arbitrary = genShape

-- * Transforming shapes

-- ** A7
-- | Rotate a shape 90 degrees
rotateShape :: Shape -> Shape
rotateShape (Shape rs) = Shape (reverse (transpose rs))

-- ** A8
-- | shiftShape adds empty squares above and to the left of the shape
shiftShape :: (Int, Int) -> Shape -> Shape
shiftShape (n1, n2) s = shiftShapeTop n1 (shiftShapeLeft n2 s)

shiftShapeTop :: Int -> Shape -> Shape
shiftShapeTop n (Shape rs) = Shape (rows (emptyShape (n, length (head rs))) 
 ++ rs)

shiftShapeLeft :: Int -> Shape -> Shape
shiftShapeLeft n (Shape rs) = Shape [ shiftRowLeft r | r <- rs ]
  where
    shiftRowLeft r = emptyRow n ++ r

-- ** A9
-- | padShape adds empty sqaure below and to the right of the shape
padShape :: (Int, Int) -> Shape -> Shape
padShape (n1, n2) s = padShapeBottom n1 (padShapeRight n2 s)

padShapeBottom :: Int -> Shape -> Shape
padShapeBottom n (Shape rs) = Shape (rs 
 ++ rows (emptyShape (n, length (head rs))) 
   )

padShapeRight :: Int -> Shape -> Shape
padShapeRight n (Shape rs) = Shape [ shiftRowRight r | r <- rs ]
  where
    shiftRowRight r = r ++ emptyRow n

-- ** A10
-- | pad a shape to a given size
padShapeTo :: (Int, Int) -> Shape -> Shape
padShapeTo (n1, n2) s = padShape (n1 - fst size, n2 - snd size ) s
 where
  size = shapeSize s 

-- * Comparing and combining shapes

-- ** B1

-- | Test if two shapes overlap
overlaps :: Shape -> Shape -> Bool
s1 `overlaps` s2 = error "A11 overlaps undefined"

-- ** B2
-- | zipShapeWith, like 'zipWith' for lists
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
zipShapeWith = error "A12 zipShapeWith undefined"

-- ** B3
-- | Combine two shapes. The two shapes should not overlap.
-- The resulting shape will be big enough to fit both shapes.
combine :: Shape -> Shape -> Shape
s1 `combine` s2 = error "A13 zipShapeWith undefined"
