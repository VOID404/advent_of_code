{-# LANGUAGE LambdaCase #-}

module Day09 where

import           Control.Exception (Exception, throw)
import           Control.Lens
import           Control.Monad     (guard)
import           Data.List         (nub, sortOn)
import           Prelude           hiding (Left, Right)

type Point = (Int, Int)

data Dir
  = Up
  | Down
  | Left
  | Right


funRepeat :: Int -> (a -> a) -> (a -> a)
funRepeat n f = foldr (.) id $ replicate n f

move :: Dir -> Point -> Point
move d (x,y) = case d of
  Up    -> (x, y + 1)
  Down  -> (x, y - 1)
  Left  -> (x - 1, y)
  Right -> (x + 1, y)

adjecant :: Point -> Point -> Bool
adjecant a b = distance a b <= 2

distance :: Point -> Point -> Int
distance (x0,y0) (x1,y1) = (x0-x1)^2 + (y0-y1)^2

data PosErr = PosErr Point Point deriving (Show)
instance Exception PosErr

fixPos :: Point -> Point -> Point
fixPos t@(x0, y0) h@(x1, y1) = case sortOn (distance t) solutions of
                                 []    -> throw $ PosErr t h
                                 (s:_) -> s
  where
    solutions = do
      x <- [-1..1]
      y <- [-1..1]

      let newTail = (x0 + x, y0 + y)

      guard $ adjecant h newTail
      guard $ (x,y) == (0,0) || distance h t == 8 || (x0+x == x1 || y0+y == y1)

      return (x0 + x, y0 + y)

parseLine :: String -> [Point -> Point]
parseLine l = case words l of
  ["R", n] -> replicate (read n) (move Right)
  ["L", n] -> replicate (read n) (move Left)
  ["U", n] -> replicate (read n) (move Up)
  ["D", n] -> replicate (read n) (move Down)

processMoves :: Point -> [Point -> Point] -> [Point]
processMoves start [] = [start]
processMoves start (m:ms) = let newPos = m start
                                in newPos : processMoves newPos ms

calculateTails :: Point -> [Point] -> [Point]
calculateTails _ [] = []
calculateTails start (h:hs) = let p = fixPos start h
                                  in p : calculateTails p hs

basic :: String -> Int
basic
  = length
  . nub
  . calculateTails (0,0)
  . processMoves (0,0)
  . concatMap parseLine
  . lines

bonus :: String -> Int
bonus
  = length
  . nub
  . funRepeat 9 (calculateTails (0,0))
  . ((0,0):)
  . processMoves (0,0)
  . concatMap parseLine
  . lines

sample :: String
sample = "R 4\n\
         \U 4\n\
         \L 3\n\
         \D 1\n\
         \R 4\n\
         \D 1\n\
         \L 5\n\
         \R 2"

largeSample :: String
largeSample = "R 5\n\
              \U 8\n\
              \L 8\n\
              \D 3\n\
              \R 17\n\
              \D 10\n\
              \L 25\n\
              \U 20"
