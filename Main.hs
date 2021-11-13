{-  This file is part of strassen-matmul.
    strassen-matmul is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    strassen-matmul is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with strassen-matmul. If not, see <https://www.gnu.org/licenses/>.  -}

import Control.DeepSeq

import Data.List

data Matrix a = Matrix { matData :: [[a]],
                         rows :: Int,
                         cols :: Int } deriving (Show)
instance NFData a => NFData (Matrix a) where
  rnf Matrix{matData = md, rows = r, cols = c} = rnf md `seq` rnf r `seq` rnf c

matInit :: (Num a, NFData a) => a -> Int -> Int -> Matrix a
matInit x r c = force $ Matrix (take r $ repeat $ take c $ repeat x) r c

matDiag :: (Num a, NFData a) => [a] -> Matrix a
matDiag [] = Matrix [] 0 0
matDiag (x:xs) = force $ Matrix ((x:(take (length xs) $ repeat 0)):(map ((:) 0) (matData $ matDiag xs))) r c
  where r = length xs + 1
        c = r

matFromList :: (Num a, NFData a) => [a] -> Int -> Int -> Maybe (Matrix a)
matFromList l r c
  | (length l) /= r * c = Nothing
  | otherwise = force $ Just $ Matrix (takeChunks c l) r c
    where takeChunks 0 _ = []
          takeChunks _ [] = []
          takeChunks s lc = (take s lc):(takeChunks s $ drop s lc)

matTranspose :: (Num a, NFData a) => Matrix a -> Matrix a
matTranspose m = force $ Matrix (transpose $ matData m) (cols m) (rows m)

matmul :: (Num a, NFData a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matmul ma mb
  | (cols ma) /= (rows mb) = Nothing
  | otherwise = force $ Just $ Matrix ([[dot r c | c <- matData tmb] | r <- (matData ma)]) (rows ma) (cols mb)
    where dot va vb = foldl' (+) 0 (zipWith (*) va vb)
          tmb = matTranspose mb

main :: IO ()
main = do
  print $ matInit (1.7 :: Float) 3 3
  print $ matDiag ([1, 1, 1] :: [Float])
  print $ matTranspose $ Matrix ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Float]]) 3 3
  print $ matmul (Matrix ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Float]]) 3 3) (Matrix ([[1, 2], [4, 5], [7, 8]] :: [[Float]]) 3 2)
  print $ matFromList ([0.4, -2.5, 5.8, 1.2, 5.4, 3.0, 1000, 20.444] :: [Float]) 4 2
