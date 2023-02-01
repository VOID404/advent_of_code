module Day10 where


data Cmd
  = Noop
  | Add Int

type Cycle = Int

parseLine :: String -> Cmd
parseLine l = case words l of
                ["noop"]    -> Noop
                ["addx", n] -> Add $ read n
                _           -> error "Invalid line"

processCmds :: Int -> [Cmd] -> [Int]
processCmds _ []   = []
processCmds x (c:cs) = case c of
                         Noop  -> x : processCmds x cs
                         Add n -> x : x+n : processCmds (x+n) cs

render :: Cycle -> Int -> Char
render c x
  | abs (c - x) <= 1 = '#'
  | otherwise        = '.'

rows :: Int -> String -> [String]
rows n s = case splitAt n s of
  (r, []) -> [r]
  (r, t)  -> r : rows n t

bonus :: String -> [String]
bonus
  = take 6
  . rows 40
  . zipWith render (map (`mod` 40) [0..])
  . (1 : )
  . processCmds 1
  . map parseLine
  . lines

basic :: String -> Int
basic
  = sum
  . map (uncurry (*))
  . filter (\(i,_) -> i `elem` [20,60..220])
  . zip [1..]
  . (1 : )
  . processCmds 1
  . map parseLine
  . lines

sample :: String
sample = "noop\n\
         \addx 3\n\
         \addx -5"

largeSample :: String
largeSample = "addx 15\n\
              \addx -11\n\
              \addx 6\n\
              \addx -3\n\
              \addx 5\n\
              \addx -1\n\
              \addx -8\n\
              \addx 13\n\
              \addx 4\n\
              \noop\n\
              \addx -1\n\
              \addx 5\n\
              \addx -1\n\
              \addx 5\n\
              \addx -1\n\
              \addx 5\n\
              \addx -1\n\
              \addx 5\n\
              \addx -1\n\
              \addx -35\n\
              \addx 1\n\
              \addx 24\n\
              \addx -19\n\
              \addx 1\n\
              \addx 16\n\
              \addx -11\n\
              \noop\n\
              \noop\n\
              \addx 21\n\
              \addx -15\n\
              \noop\n\
              \noop\n\
              \addx -3\n\
              \addx 9\n\
              \addx 1\n\
              \addx -3\n\
              \addx 8\n\
              \addx 1\n\
              \addx 5\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx -36\n\
              \noop\n\
              \addx 1\n\
              \addx 7\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx 2\n\
              \addx 6\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx 1\n\
              \noop\n\
              \noop\n\
              \addx 7\n\
              \addx 1\n\
              \noop\n\
              \addx -13\n\
              \addx 13\n\
              \addx 7\n\
              \noop\n\
              \addx 1\n\
              \addx -33\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx 2\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx 8\n\
              \noop\n\
              \addx -1\n\
              \addx 2\n\
              \addx 1\n\
              \noop\n\
              \addx 17\n\
              \addx -9\n\
              \addx 1\n\
              \addx 1\n\
              \addx -3\n\
              \addx 11\n\
              \noop\n\
              \noop\n\
              \addx 1\n\
              \noop\n\
              \addx 1\n\
              \noop\n\
              \noop\n\
              \addx -13\n\
              \addx -19\n\
              \addx 1\n\
              \addx 3\n\
              \addx 26\n\
              \addx -30\n\
              \addx 12\n\
              \addx -1\n\
              \addx 3\n\
              \addx 1\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx -9\n\
              \addx 18\n\
              \addx 1\n\
              \addx 2\n\
              \noop\n\
              \noop\n\
              \addx 9\n\
              \noop\n\
              \noop\n\
              \noop\n\
              \addx -1\n\
              \addx 2\n\
              \addx -37\n\
              \addx 1\n\
              \addx 3\n\
              \noop\n\
              \addx 15\n\
              \addx -21\n\
              \addx 22\n\
              \addx -6\n\
              \addx 1\n\
              \noop\n\
              \addx 2\n\
              \addx 1\n\
              \noop\n\
              \addx -10\n\
              \noop\n\
              \noop\n\
              \addx 20\n\
              \addx 1\n\
              \addx 2\n\
              \addx 2\n\
              \addx -6\n\
              \addx -11\n\
              \noop\n\
              \noop\n\
              \noop"
