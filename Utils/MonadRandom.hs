{-# OPTIONS_GHC -fglasgow-exts #-}

-- Taken from haskell wiki: http://www.haskell.org/hawiki/MonadRandom

module Utils.MonadRandom (
    MonadRandom,
    getRandom,
    getRandomR,uniformRandomVector,uniformRandomVectorRS,
    gaussianVector,gaussianPerturbation,gaussianPerturbationR,gaussianRand,
    normRand,
    cauchyR,
    evalRandomT,
    runRandomT,
    evalRandomIO,
    evalRand,
    fromList,
    fromNonWeightedList,
    chooseAtRandom,
    randomList,
    randomRoll,
    withRandomPair,withRandomThree, withProbability,
    Rand, RandomT -- but not the data constructors
    ) where

import Random
import Control.Monad.State
import Control.Monad.Identity
import Data.List(genericLength)
import Utils.Monad
--import System.Random.Mersenne.Pure64

class (Monad m) => MonadRandom m where
    getRandom :: (Random a) => m a
    getRandomR :: (Random a) => (a,a) -> m a

newtype (RandomGen g) => RandomT g m a = RandomT { unRT :: StateT g m a }
    deriving (Functor, Monad, MonadTrans, MonadIO)

liftState :: (MonadState s m) => (s -> (a,s)) -> m a
liftState t = do v <- get
                 let (x, v') = t v
                 put v'
                 return x

instance (Monad m, RandomGen g) => MonadRandom (RandomT g m) where
    getRandom = (RandomT . liftState) random
    getRandomR (x,y) = (RandomT . liftState) (randomR (x,y))

evalRandomT :: (RandomGen g, Monad m) => RandomT g m a -> g -> m a
evalRandomT x g = evalStateT (unRT x) g
runRandomT x g = runStateT (unRT x) g

evalRandomIO x = do
                  g <- liftIO getStdGen
                  (val,g') <- runStateT (unRT x) g
                  liftIO $ setStdGen g'
                  return val

--evalRandomMersenneIO seed x = do
--                  let g = pureMT seed
--                  (val,g') <- runStateT (unRT x) g
--                  -- liftIO $ setStdGen g'
--                  return val
--


-- Boring random monad :)
newtype Rand g a = Rand { unRand :: RandomT g Identity a }
    deriving (Functor, Monad, MonadRandom)

evalRand :: (RandomGen g) => Rand g a -> g -> a
evalRand x g = runIdentity (evalRandomT (unRand x) g)

-- functions for performing common tasks.

uniformRandomVector l = replicateM l getRandom
uniformRandomVectorRS rngs = mapM getRandomR rngs

-- Generate normally distributed numbers using box-muller transformation.
-- Result is distribution with zero mean and deviation of 1.
-- Not numerically stable

gaussianRand :: (MonadRandom m, Random a, Floating a) => m [a]
gaussianRand = do
                x1 <- getRandomR (0,1)
                x2 <- getRandomR (0,1)
                return [sqrt (-2 * log x1 ) * cos (2*pi*x2)
                       ,sqrt (-2 * log x1 ) * sin (2*pi*x2)]
                
gaussianVector sigma length = 
    replicateM (length `div` 2 + 1) gaussianRand
               >>= return.map (*sigma).take length.concat

gaussianPerturbationR sigmas lst = do
                 mutation <- gaussianVector 1 
                              (length $ lst)
                 return $ zipWith (+) lst $ zipWith (*) sigmas mutation

gaussianPerturbation sigma lst = do
                 mutation <- gaussianVector sigma 
                              (length $ lst)
                 return $ zipWith (+) lst mutation

normRand mu sigma = gaussianRand >>= return.(\x -> mu + sigma*x).head

-- fromList :: (MonadRandom m) => [(a,Rational)] -> m a
fromList [] = error "MonadRandom.fromList called with empty list"
fromList [(x,_)] = return x
fromList xs = do 
    let s = fromRational $ sum (map snd xs) -- total weight
        cs = scanl1 (\(x,q) (y,s) -> (y, s+q)) xs 
            -- cumulative weight
    p <- liftM toRational $ getRandomR (0.0,s::Double)
    return $ fst $ head $ dropWhile (\(x,q) -> q < p) cs

fromNonWeightedList l = do
                         index <- getRandomR (0,genericLength l -1)
                         return $ l !! index

chooseAtRandom (a,b) = do
    x <- getRandomR (0,1:: Int)
    if x >0 then return a else return b

randomList items n = replicateM n $ fromNonWeightedList items
randomRoll lst = do
                  i <- getRandomR (0,length lst-1)
                  return $ drop i lst ++ take i lst

withRandomPair op population = do
    a <- fromNonWeightedList population        
    b <- fromNonWeightedList population        
    op a b

withRandomThree op population = do
    a <- fromNonWeightedList population        
    b <- fromNonWeightedList population        
    c <- fromNonWeightedList population        
    op a b c

withProbability :: MonadRandom m =>  Double -> m a -> m a -> m a
withProbability p op op2 = do
    u <- getRandom
    if u<p then op else op2

cauchyR x0 gamma = do
                    y <- getRandomR (0,1::Double)
                    return $ gamma*tan(pi*(y-0.5))+x0
