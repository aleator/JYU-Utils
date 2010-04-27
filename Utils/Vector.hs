module Utils.Vector where
import Data.List
import qualified Utils.List as UL
import Numeric
-- This is a quickie module for simple vector arithmetic on lists
-- It is provided so that optimization framework is not dependent
-- on GSL

type Vector = [Double]

-- The basic arithmetic
a <> b = sum $ zipWith (*) a b
s *| v = map (*s) v
a <-> b = zipWith (-) a b 
a <+> b = zipWith (+) a b 
a <*> b = zipWith (*) a b 
v /| s = map (/s) v

infixl 7 <>, <*>, *|, /|
infixl 6 <+>, <->


average vs = map ((/genericLength vs).sum) $ transpose vs
stdDev :: [Vector] -> Double
stdDev vs = UL.average $ map (\x -> norm (x <-> avg)) vs
    where avg = average vs

-- Projecting a vector to another
proj u v = ((v <> u) / (u <> u)) *| u

-- Norms and normalization
norm x = sqrt(x<>x)
normalize x = x /| norm x

normalizeN20 = normalize . map n . normalize
 where
   n x | x >0.2    = 0.2
       | otherwise = x 

-- Gram-Schmidt orthogonalization procedure
--gramSchmidt :: [[Double]] -> [[Double]] -> [[Double]]
gramSchmidt x = gs [] x
gs us [] = reverse us 
gs [] (v:vs) = gs [v] vs
gs us (v:vs) = gs (vr:us) vs
    where vr = foldl1 (<->) $ v:[proj ui v | ui <- us]

-- Making an "identity matrix"
makeBase n = [[if y==x then 1 else 0 | x <- [1..n]] | y <- [1..n]]

-- a version of <= that works in different kind of wrong way than <=
safeLessThan a b =  ( abs (a - b) <= 1.0e-9 ) || ( a < b) 


-- Utilities for printing vectors

showVector ::Int ->  Vector -> String
showVector p l = "["++concat (intersperse "," (map (\n -> showFFloat (Just p) n "") l))++"]"

showMatrix1 :: Int -> [Vector] -> String
showMatrix1 p l = "["++concat (intersperse "," (map (showVector p) l))++"]"

showMatrix :: Int -> [Vector] -> String
showMatrix p l = "\t["++concat (intersperse ",\n\t" (map (showVector p) l))++"]"


scaleToMax l v | n > l = l *| (v /| n)
               | otherwise = v
    where
     n = norm v


-- Stuff dealing with ODE:s

rungeKutta4 h f (x0,t0) = (x0 <+> ((1/6) *| k1) 
                            <+> ((1/3) *| k2)
                            <+> ((1/3) *| k3)
                            <+> ((1/6) *| k4)
                        ,t0+h)
    where 
     k1 = h *| f x0 t0
     k2 = h *| f (x0<+> (0.5 *| k1)) (t0+h/2)
     k3 = h *| f (x0<+> (0.5 *| k2)) (t0+h/2)
     k4 = h *| f (x0<+> k3) (t0+h)

{-
class Vectorizeable a where
    toVector :: a -> Vector
    fromVector :: a -> Vector -> a

instance Vectorizeable Vector where
    toVector = id
    fromVector a = id
-}
{-
-- This is no good I think:
class ODE a b | a -> b where
    add :: a -> Double -> b -> a

addp :: (ODE a d) => a -> (Double,d) -> a
addp x (h,d) = add x h d

instance ODE Vector Vector where
    add a h d = a <+> (h*|d)
instance (ODE a b) => ODE [a] [b] where
    add a h d = zipWith (\ai di -> add ai h di) a d

eulerM h f (x0,t0) = do
                      fx <- f x0 t0
                      return (x0 <+> (h *| (f x0 t0)),t0+h)

eulerMODE :: (Monad m,ODE a d) => Double -> (a -> Double -> m d) 
                                  -> (a,Double) -> m (a,Double)
eulerMODE h f (x0,t0) = do
                      fx <- f x0 t0
                      return  (x0 `addp` (h,fx),t0+h)

rungeKutta4ODE :: (ODE point delta) => Double -> (point -> Double -> delta) 
                                        -> (point,Double) -> (point,Double)
rungeKutta4ODE h f (x0,t0) = (x0 `addp` ((h/6),k1) 
                                 `addp` ((h/3),k2)
                                 `addp` ((h/3),k3)
                                 `addp` ((h/6),k4)
                             ,t0+h)
    where 
     k1 = f x0 t0
     k2 = f (add x0 0.5 k1) (t0+h/2)
     k3 = f (add x0 0.5 k2) (t0+h/2)
     k4 = f (add x0 1 k3) (t0+h)

rungeKutta4MODE :: (Monad m, ODE point delta) => Double -> (point -> Double -> m delta) 
                                        -> (point,Double) -> m (point,Double)
rungeKutta4MODE h f (x0,t0) = do 
     k1 <- f x0 t0
     k2 <- f (add x0 0.5 k1) (t0+h/2)
     k3 <- f (add x0 0.5 k2) (t0+h/2)
     k4 <- f (add x0 1 k3) (t0+h)
     return (x0 `addp` ((h/6),k1) 
                `addp` ((h/3),k2)
                `addp` ((h/3),k3)
                `addp` ((h/6),k4)
            ,t0+h)
-}
-- Other numerical utilities. TODO: Split into own file

-- Snap a point to a grid
snap :: Double -> Double -> Double
snap s x = (fromIntegral $ round (x/is))*is where is = s

width (a,b) = abs (a-b)

