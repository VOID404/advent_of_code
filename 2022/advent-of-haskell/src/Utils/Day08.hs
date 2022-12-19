module Utils.Day08 where
import           Data.List (transpose)

type Matrix a = [[a]]

funRepeat :: Int -> (a -> a) -> (a -> a)
funRepeat n f = foldr (.) id $ replicate n f

rotate :: Matrix a -> Matrix a
rotate = reverse . transpose

zipMatrixWith :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
zipMatrixWith f = zipWith (zipWith f)

-- | calls a function on all rotations of matrix,
-- returns list of results
performRotated :: (Matrix a -> Matrix b) -> Matrix a -> [Matrix b]
performRotated f m = map (`rotated` m) [0..3]
  where
    rotated n = funRepeat (4 - n) rotate . f . funRepeat n rotate
