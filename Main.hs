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

data Matrix a = Matrix { matData :: [[a]],
                         rows :: Int,
                         cols :: Int } deriving (Show)

matInit :: (Num a) => a -> Int -> Int -> Matrix a
matInit x r c = Matrix (take r $ repeat $ take c $ repeat x) r c

matDiag :: (Num a) => [a] -> Matrix a
matDiag [] = Matrix [] 0 0
matDiag (x:xs) = Matrix ((x:(take (length xs) $ repeat 0)):(map ((:) 0) (matData $ matDiag xs))) r c
  where r = length xs + 1
        c = r

main :: IO ()
main = do
  print $ matInit (1.7 :: Float) 3 3
  print $ matDiag ([1.0, 2.0, 3.0] :: [Float])
