module Day03
  ( basic,
    bonus,
    sample,
  )
where

import           Data.Char   (isAsciiLower, isAsciiUpper)
import           Data.List   (nub)
import           GHC.Base    (divInt, ord)
import           Utils.Day03

score :: Char -> Int
score c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27
  | otherwise = error "Not a letter"

basic :: String -> Int
basic =
  sum
    . map
      ( score
          . head
          . nub
          . common
          . (\l -> splitAt (length l `divInt` 2) l)
      )
    . lines

bonus :: String -> Int
bonus =
  sum
    . map
      ( score
          . head
          . nub
          . \(a, b, c) -> common (common (a, b), c)
      )
    . tripplets
    . lines

sample :: String
sample = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
