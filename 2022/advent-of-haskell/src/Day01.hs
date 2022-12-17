module Day01
  ( basic,
    bonus,
    sample,
  )
where

import           Data.List   (sortBy)
import           Utils.Day01

basic :: String -> Int
basic = foldr max 0 . sumElves

bonus :: String -> Int
bonus =
  sum
    . take 3
    . sortBy (flip compare)
    . sumElves

sample :: String
sample = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"
