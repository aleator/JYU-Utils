module Utils.Numeric where
--import Numeric.LinearAlgebra 
import Control.Monad
import Utils.MonadRandom
import Utils.Shuffle

-- Utilities for handling of numerical lists
inBounds bounds vector = all (\((a,b),x) -> a < x && x < b) $ zip bounds vector
saturateRealsFit f bounds (_,x) = (f (zipWith saturate bounds x)
                                     ,zipWith saturate bounds x)

saturateReals bounds  x = zipWith saturate bounds x
saturateRealsM bounds x = return $ saturateReals bounds x

saturate (lb,ub) x = min (max x lb) ub

widths :: Num a => [(a,a)] -> [a]
widths = map (\(a,b) -> abs (a-b)) 


clean x | isNaN x || isInfinite x = 0
        | otherwise = x

{-scaleToUnitRows = fromRows .  map (\v -> (1/sqrt (v<.>v)) .* v ) . toRows

gramSchmidt = fromRows . gs [] . toRows
gs :: [Vector Double] -> [Vector Double] -> [Vector Double]
gs [] (v:vs) = gs [v] vs
gs us (v:vs) = gs (vr:us) vs
    where vr = foldl1 (-) $ v:[proj ui v | ui <- us]
gs us [] = reverse us -- the silly bit
proj u v = ((v <.> u) / (u <.> u)) .* u
-}

-- Generate truly orthogonal random projection matrix. 
{-
mkRandomProjMatrix1 :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
mkRandomProjMatrix1 fromD toD = do
                                 numbers <- replicateM (fromD*toD) $ getRandomR (-1,1::Double)
                                 shuffled <- doShuffle . map (\v -> (1/sqrt (v<.>v)) .* v ) 
                                                       . gs [] . toRows $ (fromD><toD)  numbers
                                 return $ fromRows shuffled

-- Generate gaussian matrix
mkRandomProjMatrix2 :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
mkRandomProjMatrix2 fromD toD = do
                                 numbers <- gaussianVector (1::Double) (fromD*toD) 
                                 return $ (fromD><toD)  numbers

-- Generate RP matrix according to Achlioptas
mkRandomProjMatrix3 :: (MonadRandom m) => Int -> Int -> m (Matrix Double)
mkRandomProjMatrix3 fromD toD = do
                                 numbers <- replicateM (fromD*toD) $ MonadRandom.fromList [(-1,1/6)
                                                                                          ,(0 ,2/3)
                                                                                          ,(1 ,1/6)]
                                 return $ (fromD><toD)  numbers
-}            
