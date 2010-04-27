module Utils.Sampling where

import Utils.SemanticEditors
import Control.Arrow
import Control.Monad
import Data.List
import Utils.List
import Utils.MonadRandom
import Utils.Shuffle

-- This module is for separating sampling from MonadRandom in hopes of some day replacing it with
-- the standard version.

-- Uniform probability selection
selectRandom n l = replicateM n $ fromNonWeightedList l

-- Stochastic universal sampling
-- Results are randomized order
doSUS no weightedList = do
    x <- getRandomR (0,1) -- random spin
    doShuffle (sus weightedList no x)

sus :: [(Double,a)] -> Int -> Double -> [a]
sus elements no offsetRatio = sus' pts es []  
 where
    offset = offsetRatio*armSpan
    armSpan = totalWeight/fromIntegral no
    es = ((inzip first (scanl1 (+)).cycle.sortBy (comparing (negate.fst)))  elements)
    pts = [min totalWeight (offset+i) | i <- take no [0,armSpan..]]
    totalWeight = sum.map fst $ elements

sus' [] _ acc = acc
sus' a@(p:pts) r@((w,e):es) acc 
    | p <= w  = sus' pts r  (e:acc)
    | p > w  = sus' a es acc
    | otherwise = sus' pts r acc
