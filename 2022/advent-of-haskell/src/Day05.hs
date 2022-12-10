module Day05
  ( basic,
    bonus,
    sample,
  )
where

import Control.Exception (Exception, throw)
import Control.Lens
import Data.Char (isAlpha, isAlphaNum)
import Data.List (transpose)

data BadStack = BadStack [String] Move deriving (Show)

instance Exception BadStack

basic :: String -> String
basic input = map head $ foldl (flip perform) stacks moves
  where
    (stacks, moves) = case break (== "") $ lines input of
      (s, "" : m) -> (parseStacks s, parseMoves m)
      _ -> error "Invalid input"

bonus :: String -> String
bonus input = map head $ foldl (flip perform2) stacks moves
  where
    (stacks, moves) = case break (== "") $ lines input of
      (s, "" : m) -> (parseStacks s, parseMoves m)
      _ -> error "Invalid input"

parseStacks :: [String] -> [String]
parseStacks =
  map (filter isAlpha)
    . filter (any isAlphaNum)
    . transpose

data Move = Move
  { amount :: Int,
    from :: Int,
    to :: Int
  }
  deriving (Show)

parseMove :: String -> Move
parseMove s = case words s of
  ["move", n, "from", a, "to", b] -> Move (read n) (read a - 1) (read b - 1)
  _ -> error "Bad move line"

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

perform2 :: Move -> [String] -> [String]
perform2 (Move 0 _ _) stacks = stacks
perform2 (Move n from to) stacks = newStacks
  where
    (moved, rest) = splitAt n (stacks !! from)
    toStack = stacks !! to
    newStacks = stacks & ix to .~ (moved ++ toStack) & ix from .~ rest

perform :: Move -> [String] -> [String]
perform (Move 0 _ _) stacks = stacks
perform (Move n from to) stacks = perform (Move (n - 1) from to) newStacks
  where
    (x : frStack) = stacks !! from
    toStack = stacks !! to
    newStacks = stacks & ix to .~ (x : toStack) & ix from .~ frStack

sample :: String
sample = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
