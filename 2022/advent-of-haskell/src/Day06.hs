module Day06
  ( basic,
    bonus,
    samples,
    results,
    resultsBonus,
  )
where

basic :: String -> Int
basic = (+ 4) . indexOf allUnique . window 4

bonus :: String -> Int
bonus = (+ 14) . indexOf allUnique . window 14

window :: Int -> [a] -> [[a]]
window n l = windowR n l []
  where
    windowR n l acc =
      if length arr == n
        then windowR n (tail l) (acc ++ [arr])
        else acc
      where
        arr = take n l

countIf :: (Foldable t) => (a -> Bool) -> t a -> Int
countIf f = foldr count 0
  where
    count a b = if f a then b + 1 else b

unique :: (Foldable t, Eq a) => a -> t a -> Bool
unique e l = (countIf (== e) l) <= 1

allUnique :: (Foldable t, Eq a) => t a -> Bool
allUnique l = all (`unique` l) l

indexOf :: (a -> Bool) -> [a] -> Int
indexOf f l = recur l 0
  where
    recur [] _ = -1
    recur (h : t) i = if f h then i else recur t (i + 1)

samples :: [String]
samples =
  [ "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
    "bvwbjplbgvbhsrlpgdmjqwftvncz",
    "nppdvjthqldpwncqszvftbrmjlhg",
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
  ]

results :: [Int]
results = [7, 5, 6, 10, 11]

resultsBonus :: [Int]
resultsBonus = [19, 23, 23, 29, 26]
