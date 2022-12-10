module Day03
  ( basic,
    bonus,
    sample,
  )
where

import Data.List (nub)
import GHC.Base (divInt, ord)

common :: Eq a => ([a], [a]) -> [a]
common (l1, l2) = commonR l1 []
  where
    commonR [] acc = acc
    commonR (a : as) acc
      | a `elem` l2 = commonR as (a : acc)
      | otherwise = commonR as acc

score c
  | c >= 'a' && c <= 'z' = (ord c - ord 'a') + 1
  | c >= 'A' && c <= 'Z' = (ord c - ord 'A') + 27
  | otherwise = error "Not a letter"

tripplets :: [c] -> [(c, c, c)]
tripplets l = tripR l []
  where
    tripR (a : b : c : t) acc = tripR t ((a, b, c) : acc)
    tripR _ acc = acc

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
