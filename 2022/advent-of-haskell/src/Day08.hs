{-# LANGUAGE TemplateHaskell #-}

module Day08 where

import           Control.Lens
import           Data.Char    (digitToInt)
import           Data.List


toTrees :: [String] -> [[Int]]
toTrees ls = ls <&> (<&> digitToInt)

shortMap :: [Int] -> [Bool]
shortMap = go (-1)
  where
    go :: Int -> [Int] -> [Bool]
    go _ []     = []
    go m (t:ts) = (t > m) : go (max t m) ts

funRepeat :: Int -> (a -> a) -> (a -> a)
funRepeat n f = foldr (.) id $ replicate n f

visMatrix :: [[Int]] -> [[Bool]]
visMatrix ts = mergedMask
  where
    sides :: [[[Bool]]]
    sides = map (`rotatedShorts` ts) [0..3]

    rotate = reverse . transpose

    rotatedShorts :: Int -> [[Int]] -> [[Bool]]
    rotatedShorts n = funRepeat (4 - n) rotate . map shortMap . funRepeat n rotate

    mergeMasks :: [[Bool]] -> [[Bool]] -> [[Bool]]
    mergeMasks = zipWith (zipWith (||))

    mergedMask = foldr1 mergeMasks sides

basic :: String -> Int
basic = length . filter id . concat . visMatrix . toTrees . lines

sample :: String
sample = "30373\n\
         \25512\n\
         \65332\n\
         \33549\n\
         \35390"
