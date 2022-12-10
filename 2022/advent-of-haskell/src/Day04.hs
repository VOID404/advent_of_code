module Day04
  ( basic,
    bonus,
    sample,
  )
where

import Control.Exception (Exception, throw)
import Data.Char (isDigit)
import Data.List (isSubsequenceOf)

basic :: String -> Int
basic = length . filter overlap . lines
  where
    overlap = (\(a, b) -> a `isSubsequenceOf` b || b `isSubsequenceOf` a) . parseLn

bonus :: String -> Int
bonus = length . filter overlap . lines
  where
    overlap = (\(a, b) -> any (`elem` b) a) . parseLn

type Line = ([Int], [Int])

newtype Error = BadLine String deriving (Show, Eq)

instance Exception Error

parseLn :: String -> Line
parseLn l =
  case readInt l [] of
    [l1, t1, l2, t2] -> ([l1 .. t1], [l2 .. t2])
    _ -> throw $ BadLine l
  where
    readInt [] acc = reverse acc
    readInt s acc =
      let num = read $ takeWhile isDigit s :: Int
          rest =
            dropWhile (not . isDigit) $
              dropWhile isDigit s
       in readInt rest (num : acc)

sample :: String
sample = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"
