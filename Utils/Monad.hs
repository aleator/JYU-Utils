module Utils.Monad where
import Control.Monad
import Control.Parallel.Strategies(demanding,rnf)

-- Return result evaluated to rnf
bangReturn x = return x `demanding` rnf x

-- Monadic if
conditional cond thenOp elseOp = do
    t <- cond
    if t then thenOp
         else elseOp

whenM cond thenOp = do
    t <- cond
    if t then thenOp
         else return ()


pairM a b = do
        x <- a
        y <- b
        return (x,y)

sequenceWithPar init [] = init
sequenceWithPar init (op:ops) = sequenceWithPar (op init) ops

sequenceWithParM :: (Monad m) => a -> [a -> m a] -> m a
sequenceWithParM init [] = return init
sequenceWithParM init (op:ops) = do
    r <- op init
    sequenceWithParM r ops

scanJoinMaybe s ops = do
                  sr <- s 
                  cs sr ops [] 
    where 
     cs start (op:ops) res = do  
                         r <- op start
                         case r of
                          Just r' -> cs r' ops (start:res)
                          Nothing -> return $ reverse $ start:res
     cs start [] res = return $ reverse $ start:res 

scanJoin s ops = do
                  sr <- s 
                  cs sr ops [] 
    where 
     cs start (op:ops) res = do  
                         r <- op start
                         cs r ops (start:res)
     cs start [] res = return $ reverse $ start:res 
                                                 

-- chain ms = foldl1 (>=>) ms

doWhile :: (Monad m) => (a -> m (Maybe a)) -> a -> m a
doWhile op x = do
                r <- op x
                case r of 
                    Just b  -> do
                                a <- doWhile op b
                                return a
                    Nothing -> return x

nTimes  n op = sequence $ replicate n op
nTimes_ n op = sequence_ $ replicate n op

-- Keep performing action `op` until its result satisfies cond
untilM :: (Monad m) => (a -> Bool) -> (m a) -> m a
untilM cond op = do
            x <- op
            if cond x then return x else untilM cond op

repeatMLast n initial op = foldM join (initial) (replicate n op) >>= return.reverse
    where 
     join a b = b a
                

repeatM n initial op = foldM join [initial] (replicate n op) >>= return.reverse
    where 
     join a b = do
                 r <- b $ head a
                 return $ r:a

iterateWhileM_ op cond x = recurse x 
    where
     recurse p = do
                r <- op p
                if cond r 
                    then recurse r
                    else return ()

iterateWhileM op cond x = recurse x [x] 
    where
     recurse p acc = do
                r <- op p
                if cond r 
                    then recurse r (r:acc)
                    else return $ r:acc

stepWhile op x = sw x [] 
    where
     sw p acc = do
                r <- op p
                case r of 
                    Just b  -> sw b (p:acc)
                    Nothing -> p:acc
