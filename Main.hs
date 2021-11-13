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
import Control.Exception

import Data.Bits
import Data.List
import Data.Maybe

import GHC.Integer.Logarithms

import System.Random

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

matmulNaive :: (Num a, NFData a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matmulNaive ma mb
  | (cols ma) /= (rows mb) = Nothing
  | otherwise = force $ Just $ Matrix ([[dot r c | c <- matData tmb] | r <- (matData ma)]) (rows ma) (cols mb)
    where dot va vb = foldl' (+) 0 (zipWith (*) va vb)
          tmb = matTranspose mb

matmulStrassen :: (Num a, NFData a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matmulStrassen ma mb
  | (cols ma) /= (rows mb) = Nothing
  | otherwise = force $ Just $ Matrix mbPow2 (rows ma) (cols mb)
    where maPow2 = (map (\x -> x ++ (take (nextPow2 ca - ca) $ repeat 0)) $ matData ma) ++ (take (nextPow2 ra - ra) $ repeat $ take (nextPow2 ca) $ repeat 0)
          mbPow2 = (map (\x -> x ++ (take (nextPow2 cb - cb) $ repeat 0)) $ matData mb) ++ (take (nextPow2 rb - rb) $ repeat $ take (nextPow2 cb) $ repeat 0)
          ra = rows ma
          ca = cols ma
          rb = rows mb
          cb = cols mb
          nextPow2 0 = 0
          nextPow2 x = 2 ^ (countLeadingZeros (0 :: Int) - countLeadingZeros (x - 1))

main :: IO ()
main = do
  g <- newStdGen
  a <- return $!! fromJust $ matFromList (take 15 (randoms g :: [Double])) 3 5
  g <- newStdGen
  b <- return $!! fromJust $ matFromList (take 10 (randoms g :: [Double])) 5 2
  c <- return $!! matmulStrassen a b
  print b
  print c
