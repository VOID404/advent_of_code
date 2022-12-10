module Day01
  ( basic,
    bonus,
    sample,
  )
where

import Control.Arrow ((>>>))
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

basic :: String -> Int
basic = foldr max 0 . sumElves

bonus :: String -> Int
bonus =
  sum
    . take 3
    . sortBy (flip compare)
    . sumElves

split :: Eq a => a -> [a] -> [[a]]
split del list = splitR list []
  where
    splitR l acc = case break (== del) l of
      (a, _ : b) -> splitR b (a : acc)
      (a, _) -> a : acc

sumElves :: String -> [Int]
sumElves =
  lines
    >>> (map readMaybe :: [String] -> [Maybe Int])
    >>> split Nothing
    >>> map sequence
    >>> fromJust . sequence
    >>> map sum

sample :: String
sample = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
