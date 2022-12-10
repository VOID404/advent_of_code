module Day02
  ( basic,
    bonus,
    sample,
  )
where

import Control.Exception (Exception, throw)
import Control.Monad (guard)

basic :: String -> Int
basic = sum . map (score . parseLn) . lines

bonus :: String -> Int
bonus = sum . map (score . parseStragy . parseLn) . lines

data RPS = Rock | Paper | Scissors
  deriving (Show, Eq)

data ParseException
  = InvalidChar Char
  | InvalidLine String
  deriving (Show)

instance Exception ParseException

parseLn :: String -> (RPS, RPS)
parseLn [a, ' ', b] = (parseChar a, parseChar b)
  where
    parseChar c = case c of
      _ | c `elem` ['A', 'X'] -> Rock
      _ | c `elem` ['B', 'Y'] -> Paper
      _ | c `elem` ['C', 'Z'] -> Scissors
      _ -> throw $ InvalidChar c
parseLn l = throw $ InvalidLine l

piece :: RPS -> Int
piece a = case a of
  Rock -> 1
  Paper -> 2
  Scissors -> 3

loose :: RPS -> RPS -> Bool
loose a b = case (a, b) of
  (Paper, Rock) -> True
  (Rock, Scissors) -> True
  (Scissors, Paper) -> True
  _ -> False

win :: RPS -> RPS -> Bool
win = flip loose

score :: (RPS, RPS) -> Int
score (a, b)
  | win a b = 6 + piece b
  | a == b = 3 + piece b
  | otherwise = 0 + piece b

parseStragy :: (RPS, RPS) -> (RPS, RPS)
parseStragy (enemy, result) = case pstr of
  [a] -> (enemy, a)
  _ -> error "Couldn't find requested strategy"
  where
    pstr = do
      my <- [Rock, Paper, Scissors]
      guard $ case result of
        Rock -> loose enemy my
        Paper -> my == enemy
        Scissors -> win enemy my
      return my

sample :: String
sample = "A Y\nB X\nC Z"
