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

import System.Random

data Matrix a = Matrix { matData :: [[a]],
                         rows :: Int,
                         cols :: Int } deriving (Show)
instance NFData a => NFData (Matrix a) where
  rnf Matrix{matData = md, rows = r, cols = c} = rnf md `seq` rnf r `seq` rnf c

matInit :: (Num a, NFData a) => a -> Int -> Int -> Matrix a
matInit x r c = id $!! Matrix (take r $ repeat $ take c $ repeat x) r c

matDiag :: (Num a, NFData a) => [a] -> Matrix a
matDiag [] = Matrix [] 0 0
matDiag (x:xs) = id $!! Matrix ((x:(take (length xs) $ repeat 0)):(map ((:) 0) (matData $ matDiag xs))) r c
  where r = length xs + 1
        c = r

matFromList :: (Num a, NFData a) => [a] -> Int -> Int -> Maybe (Matrix a)
matFromList l r c
  | (length l) /= r * c = Nothing
  | otherwise = Just $!! Matrix (takeChunks c l) r c
    where takeChunks 0 _ = []
          takeChunks _ [] = []
          takeChunks s lc = (take s lc):(takeChunks s $ drop s lc)

matTranspose :: (Num a, NFData a) => Matrix a -> Matrix a
matTranspose m = force $ Matrix (transpose $ matData m) (cols m) (rows m)

matmulNaive :: (Num a, NFData a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matmulNaive ma mb
  | (cols ma) /= (rows mb) = Nothing
  | otherwise = Just $!! Matrix ([[dot r c | c <- matData tmb] | r <- (matData ma)]) (rows ma) (cols mb)
    where dot va vb = foldl' (+) 0 (zipWith (*) va vb)
          tmb = matTranspose mb

leafCutoffSize = 100 :: Int

matmulStrassenInternal :: (Num a, NFData a) => [[a]] -> [[a]] -> Int -> Int -> Int -> Int -> [[a]]
matmulStrassenInternal ma mb ra ca rb cb
  | ra <= leafCutoffSize || ca <= leafCutoffSize || rb <= leafCutoffSize || cb <= leafCutoffSize = [[(\x y -> foldl' (+) 0 (zipWith (*) x y)) r c | c <- transpose mb] | r <- ma]
  | otherwise = id $!! assemble4 c1 c2 c3 c4
  where assemble4 m1 m2 m3 m4 = (zipWith (++) m1 m2) ++ (zipWith (++) m3 m4)
        anr = ra `div` 2
        anc = ca `div` 2
        bnr = rb `div` 2
        bnc = cb `div` 2
        a11 = take anr $ map (take anc) ma
        a12 = take anr $ map (drop anc) ma
        a21 = drop anr $ map (take anc) ma
        a22 = drop anr $ map (drop anc) ma
        b11 = take bnr $ map (take bnc) mb
        b12 = take bnr $ map (drop bnc) mb
        b21 = drop bnr $ map (take bnc) mb
        b22 = drop bnr $ map (drop bnc) mb
        matAdd = zipWith (zipWith (+))
        matSub = zipWith (zipWith (-))
        p = matmulStrassenInternal (a11 `matAdd` a22) (b11 `matAdd` b22) anr anc bnr bnc
        q = matmulStrassenInternal (a21 `matAdd` a22) b11 anr anc bnr bnc
        r = matmulStrassenInternal a11 (b12 `matSub` b22) anr anc bnr bnc
        s = matmulStrassenInternal a22 (b21 `matSub` b11) anr anc bnr bnc
        t = matmulStrassenInternal (a11 `matAdd` a12) b22 anr anc bnr bnc
        u = matmulStrassenInternal (a21 `matSub` a11) (b11 `matAdd` b12) anr anc bnr bnc
        v = matmulStrassenInternal (a12 `matSub` a22) (b21 `matAdd` b22) anr anc bnr bnc
        c1 = ((p `matAdd` s) `matSub` t) `matAdd` v
        c2 = r `matAdd` t
        c3 = q `matAdd` s
        c4 = ((p `matAdd` r) `matSub` q) `matAdd` u

matmulStrassen :: (Num a, NFData a) => Matrix a -> Matrix a -> Maybe (Matrix a)
matmulStrassen ma mb
  | (cols ma) /= (rows mb) = Nothing
  | otherwise = Just $!! Matrix (trimExcess $ matmulStrassenInternal maPow2 mbPow2 (nextPow2 ra) (nextPow2 ca) (nextPow2 rb) (nextPow2 cb)) ra cb
    where maPow2 = (map (\x -> x ++ (take (nextPow2 ca - ca) $ repeat 0)) $ matData ma) ++ (take (nextPow2 ra - ra) $ repeat $ take (nextPow2 ca) $ repeat 0)
          mbPow2 = (map (\x -> x ++ (take (nextPow2 cb - cb) $ repeat 0)) $ matData mb) ++ (take (nextPow2 rb - rb) $ repeat $ take (nextPow2 cb) $ repeat 0)
          ra = rows ma
          ca = cols ma
          rb = rows mb
          cb = cols mb
          nextPow2 0 = 0
          nextPow2 x = 2 ^ (countLeadingZeros (0 :: Int) - countLeadingZeros (x - 1))
          trimExcess = take ra . (map $ take cb)

main :: IO ()
main = do
  g <- newStdGen
  a <- return $!! fromJust $ matFromList (take 1000000 (randoms g :: [Double])) 1000 1000
  g <- newStdGen
  b <- return $!! fromJust $ matFromList (take 1000000 (randoms g :: [Double])) 1000 1000
  c <- return $!! matmulStrassen a b
  d <- return $!! matmulNaive a b
  print $!! foldl' (+) 0 $ map (foldl' (+) 0) $ matData $ fromJust c
  print $!! foldl' (+) 0 $ map (foldl' (+) 0) $ matData $ fromJust d
  return ()
