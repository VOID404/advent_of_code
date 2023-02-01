{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Day07
  ( basic
  , bonus
  , sample
  )
where
import           Control.Applicative   ((<|>))
import           Data.Foldable         (Foldable (fold))
import           Data.Generics.Product (HasField (field))
import           GHC.Generics          (Generic)
import           Optics

data Tree a b
  = Tree { crumb    :: a
         , values   :: [b]
         , branches :: [Tree a b]
         } deriving (Show, Generic, Functor, Foldable, Traversable)


type instance Index (Tree a b) = [a]
type instance IxValue (Tree a b) = Tree a b

instance Eq a => At (Tree a b) where
  at p = lens (get p) (set p)
    where
      get [] _ = Nothing
      get [p0] t@Tree{crumb = p1}
        | p1 == p0   = Just t
        | otherwise = Nothing
      get (p0:ps) Tree {crumb = p1, branches = bs}
        | p1 == p0   = foldr (\t acc -> acc <|> get ps t) Nothing bs
        | otherwise = Nothing
      set [] t _ = t
      set _ t Nothing = t
      set [p0] o@Tree{crumb = p1} (Just t)
        | p1 == p0   = t
        | otherwise = o
      set (p0:ps) o@Tree{crumb = p1} t@(Just _)
        | p1 == p0 = o & field @"branches" % mapped %~ (\b -> set ps b t)
        | otherwise = o

instance Eq a => Ixed (Tree a b)

treeSize :: Tree String FS -> [Int]
treeSize Tree{ branches = bs
             , values = fs}
  = files + sum (map head children) : fold children
  where
    size (File s _) = s
    size (Dir _ )   = 0
    files = sum $ fs <&> size
    children = map treeSize bs

ensurePath :: Eq a => [a] -> Tree a b -> Tree a b
ensurePath p t = case t ^? ix p of
                   Nothing -> t & ix (noLast p)
                                % field @"branches"
                                %~ (Tree{ crumb = last p
                                        , branches = []
                                        , values = []} :)
                   Just _  -> t
                where
                  noLast []     = []
                  noLast [_]    = []
                  noLast (x:xs) = x : noLast xs

                  last []     = error "can't get last of empty list"
                  last [x]    = x
                  last (_:xs) = last xs

data FS
  = File Int String
  | Dir String
  deriving (Show, Eq)

data Command
  = CdRoot
  | CdUp
  | Cd String
  | LS [FS]
  deriving (Show, Eq)

data WalkState = WalkState { _dir     :: [String]
                           , _dirTree :: Tree String FS
                           } deriving (Show)

makeLenses ''WalkState

parseFS :: [String] -> [FS]
parseFS = map pfs
  where
    pfs l = case words l of
      ["dir", dirname] -> Dir dirname
      [size, filename] -> File (read size) filename
      _                -> error "Invalid FS line"

parseCommands :: [String] -> Tree String FS
parseCommands lines = go lines (WalkState { _dir = []
                                 , _dirTree = Tree "" [] []})
                      ^. dirTree
  where
    isCmd (h:_) = h == '$'
    isCmd _     = False

    go [] state     = state
    go ls state = case pcmd ls of
      (CdRoot, t) -> go t (state & dir .~ [""])
      (CdUp, t)   -> go t (state & dir %~ tail)
      (Cd d, t) -> let path = d : state ^. dir
                       in go t (state & dir .~ path & dirTree %~ ensurePath (reverse path))
      (LS l, t) -> let newState = state & dirTree
                                        % ix (state ^. dir % to reverse)
                                        % field @"values" %~ (l ++)
                       in go t newState

    pcmd [] = error "Can't parse no lines as command"
    pcmd (l:ls) -- parse a command
      | isCmd l = case words l of
                    ["$", "cd", "/"]  -> (CdRoot, ls)
                    ["$", "cd", ".."] -> (CdUp, ls)
                    ["$", "cd", d]  -> (Cd d, ls)
                    ["$", "ls"]  -> let (fsRaw, rest) = break isCmd ls
                                        fs = parseFS fsRaw
                                        in (LS fs, rest)
                    _            -> error "Bad command"
      | otherwise = error "Line is not a command"

basic :: String -> Int
basic = sum
        . filter (100000>)
        . treeSize
        . parseCommands
        . lines

bonus :: String -> Int
bonus is = foldr min maxBound
           . filter (>=needed) $ sizes
        where
          sizes =
            treeSize
            . parseCommands
            . lines $ is
          needed = 30000000 - (70000000 - foldr max 0 sizes)

sample :: String
sample =
  "$ cd /\n\
  \$ ls\n\
  \dir a\n\
  \14848514 b.txt\n\
  \8504156 c.dat\n\
  \dir d\n\
  \$ cd a\n\
  \$ ls\n\
  \dir e\n\
  \29116 f\n\
  \2557 g\n\
  \62596 h.lst\n\
  \$ cd e\n\
  \$ ls\n\
  \584 i\n\
  \$ cd ..\n\
  \$ cd ..\n\
  \$ cd d\n\
  \$ ls\n\
  \4060174 j\n\
  \8033020 d.log\n\
  \5626152 d.ext\n\
  \7214296 k\n"
