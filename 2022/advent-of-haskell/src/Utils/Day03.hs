module Utils.Day03 where

common :: Eq a => ([a], [a]) -> [a]
common (l1, l2) = commonR l1 []
  where
    commonR [] acc = acc
    commonR (a : as) acc
      | a `elem` l2 = commonR as (a : acc)
      | otherwise = commonR as acc

tripplets :: [c] -> [(c, c, c)]
tripplets l = tripR l []
  where
    tripR (a : b : c : t) acc = tripR t ((a, b, c) : acc)
    tripR _ acc               = acc
