{-# OPTIONS -fglasgow-exts #-}
module Utils.String where

import Numeric
import Test.QuickCheck

-- Utilities for printing numbers
    
showPercentage n = showFFloat (Just 2) n ""


-- TODO: Unify the three below
columns :: [(String,String)] -> String
columns pairs = unlines $ cd
    where
     fstLen = maximum $ map (length.fst) pairs 
     cd = map (\(a,b) -> padToR ' ' fstLen a++"\t"++b) pairs

columnS :: (Show a) => [(String,a)] -> String
columnS pairs = unlines $ cd
    where
     fstLen = maximum $ map (length.fst) pairs 
     cd = map (\(a,b) -> padToR ' ' fstLen a++"\t"++show b) pairs

columnsBy show pairs = unlines $ cd
    where
     fstLen = maximum $ map (length.fst) pairs 
     cd = map (\(a,b) -> padToR ' ' fstLen a++"\t"++show b) pairs

-- Pad string/list `s` to length `width` using `padding`
padToR pad width s = s ++ padding 
        where 
         padding = replicate n pad
         n = max 0 (width - length s)
padTo pad width s = padding ++ s 
        where 
         padding = replicate n pad
         n = max 0 (width - length s)

prop_padLength (w::Int) (str::[Int])  = 
    length (padTo 0 w str) >= length str
    && length (padTo 0 w str) >= w

quote str = '"':str++"\""

enumerate strs = zipWith (++) strs [show i | i<-[1..]]
numbered str = enumerate (repeat str)

wordsBy p s
  | findSpace == [] = []
  | otherwise = w : wordsBy p s''
  where
  (w, s'') = break p findSpace
  findSpace = dropWhile p s

printLabels ls = putStrLn $ concatMap (\(a,b) -> a++":\t"++show b++"\n") ls
 
