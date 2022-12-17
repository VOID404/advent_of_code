module Utils.Day01 where

import           Data.Maybe (fromJust)
import           Text.Read  (readMaybe)

split :: Eq a => a -> [a] -> [[a]]
split del list = case break (== del) list of
                   (a, _ : b) -> a : split del b
                   (a, _ ) -> a : []

sumElves :: String -> [Int]
sumElves = map sum
            . fromJust
            . mapM sequence . split Nothing
            . (map readMaybe  :: [String] -> [Maybe Int])
            . lines
