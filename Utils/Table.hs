module Utils.Table where
import Prelude hiding (map)
import Control.Monad
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Function

type Table row col c = Map.Map (row,col) c
type Stripe a b  = Map.Map a b

listTable lst = Map.fromList . concat $ [[((u,v),x) | x <- as | u <- [1..]] | as <- lst | v <- [1..]] 

fromList x = Map.fromList x
toList   x = Map.toList x

elems = Map.elems
indexes = Map.keys

intersectionWith :: (Ord row, Ord col) => (a -> b -> c) 
                -> Table row col a
                -> Table row col b
                -> Table row col c
intersectionWith = Map.intersectionWith

-- map   :: (c -> d) -> Table a b c -> Table a b d
map f = Map.map f 
mapM f = seqTable . map f 

-- mapI :: ((a,b) -> c -> d) -> Table a b c -> Table a b d
mapI f = Map.mapWithKey f
mapIM f = seqTable . mapI f
mapIM_ f = seqTable_ . mapI f

row    :: (Ord a, Ord b) => a -> Table a b c -> Stripe b c
row rowName table = rows table Map.! rowName 

column :: (Ord a, Ord b) => b -> Table a b c -> Stripe a c
column colName table = columns table Map.! colName 

rows :: (Ord a, Ord b) => Table a b c -> Map.Map a (Stripe b c)
rows = getStripe (snd,fst)

columns :: (Ord a, Ord b) => Table a b c -> Map.Map b (Stripe a c)
columns = getStripe (fst,snd)

rowNames :: (Ord a, Ord b) => Table a b c -> [a]
rowNames = L.nub . L.map fst . Map.keys

colNames :: (Ord a, Ord b) => Table a b c -> [b]
colNames = L.nub . L.map snd . Map.keys -- This is inefficient (nub)


-- fromRows expects to form a full matrix
fromRows :: (Ord a, Ord b) =>  Map.Map a (Stripe b c) -> Table a b c
fromRows rs = Map.fromList
              [((r,c),rs Map.! r Map.! c) 
              | r <- Map.keys rs 
              , c <- L.nub $ L.concatMap Map.keys $ Map.elems rs]

fromCols :: (Ord a, Ord b) =>  Map.Map a (Stripe b c) -> Table b a c
fromCols rs = Map.fromList
              [((c,r),rs Map.! r Map.! c) 
              | r <- Map.keys rs 
              , c <- L.nub $ L.concatMap Map.keys $ Map.elems rs]
--

namedGroupsBy op = L.map (\x -> (op (head x),x))
                   . L.groupBy (\x y -> op x == op y)
                   . L.sortBy (compare `on` op)

getStripe (want,lose) table 
        =    Map.fromList
             . L.map makeStripe
             . L.map dropR
             . namedGroupsBy (\(k,_) -> lose k)
             . Map.toList $ table
    where 
     dropR (n,s) = (n,L.map (\(k,v) -> (want k,v)) s)
     makeStripe (s,g) = (s,(Map.fromList g))


-- seqTable :: (Ord a,Ord b,Monad m) => Table a b (m c) -> m (Table a b c)
seqTable table = Control.Monad.mapM (\(k,v) -> v >>= \r-> return (k,r)) (Map.toList table)
                  >>= return . Map.fromList

seqTable_ table = Control.Monad.mapM_
                  (\(k,v) -> v) (Map.toList table)

