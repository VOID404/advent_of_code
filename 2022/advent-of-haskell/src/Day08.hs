module Day08 where

import Optics
import           Data.Char    (digitToInt)
import           Utils.Day08

toTrees :: [String] -> Matrix Int
toTrees ls = ls <&> (<&> digitToInt)

shortMap :: [Int] -> [Bool]
shortMap = go (-1)
  where
    go :: Int -> [Int] -> [Bool]
    go _ []     = []
    go m (t:ts) = (t > m) : go (max t m) ts

scenicMap :: [Int] -> [Int]
scenicMap = go []
  where
    go :: [Int] -> [Int] -> [Int]
    go _ []      = []
    go lt (t:rt) = countView lt t : go (t : lt) rt

countView :: [Int] -> Int -> Int
countView = go 0
  where
    go acc [] _ = acc
    go acc (l:ls) t
      | l < t  = go (acc + 1) ls t
      | l == t = acc + 1
      | l > t  = acc + 1

visMatrix :: Matrix Int -> Matrix Bool
visMatrix ts = mergedMask
  where
    sides :: [Matrix Bool]
    sides = performRotated (map shortMap) ts

    mergedMask = foldr1 (zipMatrixWith (||)) sides

scenicMatrix :: Matrix Int -> Matrix Int
scenicMatrix ts = mergedMask
  where
    sides :: [Matrix Int]
    sides = performRotated (map scenicMap) ts

    mergedMask = foldr1 (zipMatrixWith (*)) sides

basic :: String -> Int
basic = length . filter id . concat . visMatrix . toTrees . lines

bonus :: String -> Int
bonus = maximum . concat . scenicMatrix . toTrees . lines

sample :: String
sample = "30373\n\
         \25512\n\
         \65332\n\
         \33549\n\
         \35390"
