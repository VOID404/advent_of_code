module Utils.Day08 where
import           Data.List (transpose)

type Matrix a = [[a]]

funRepeat :: Int -> (a -> a) -> (a -> a)
funRepeat n f = foldr (.) id $ replicate n f

rotate :: Matrix a -> Matrix a
rotate = reverse . transpose
