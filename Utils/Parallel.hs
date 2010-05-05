module Utils.Parallel where
import Data.IORef
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import Control.Monad

import Utils.Monad
import Utils.File

-- Module for running simple tasks in parallel
-- Fork with indicator when the action completes
-- myForkOS :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar 
    forkIO (io `finally`  (putMVar mvar ()) )
    return mvar

-- Run IO () actions in parallel. It is assumed that the return values are
-- stored elsewhere

tvarTake x n = do 
    l <- readTVar x
    let (res,rem) = (take n l,drop n l)
    writeTVar x rem
    return res

tvarPop x = do 
    l <- readTVar x
    writeTVar x $ tail l
    return $ head l

tvarPush x t = do 
    l <- readTVar x
    writeTVar x $ l ++ [t]
    return $ head l

liftT f = \var -> do
            v <- readTVar var
            f v

data TaskRunnerStrategy = Persistent | Failing deriving(Eq,Show) 

taskRunner :: TaskRunnerStrategy -> TVar Bool -> TVar [IO ()] -> IO ()
taskRunner s terminated tasks = complete (Just $ return ())
 where
  catch :: IO () -> IOException -> IO ()
  catch t e = case s of 
                     Persistent -> do
                                    atomically (tvarPush tasks t)
                                    strictAppendFile "PAR-RUN-ERRORS" ("persisting: "++show e++"\n")
                                        `E.catch` (\err -> error $ "Error adding to PAR-RUN-ERRORS "
                                                        ++show (err:: IOException)++". Tried to add "
                                                        ++show e)
                                        
                     Failing -> strictAppendFile "PAR-RUN-ERRORS" ("failing: "++show e++"\n") 
                                        `E.catch` (\err -> error $ "Error adding to PAR-RUN-ERRORS "
                                                        ++show (err:: IOException)++". Tried to add "
                                                        ++show e)

  
  complete (Just task) = do 
                    task `E.catch` (catch task)
                    newTask <- next
                    complete newTask
  complete Nothing = atomically $ writeTVar terminated True

  next = atomically $ do
                anyLeft <- (liftT (return.not.null)) tasks
                if anyLeft then tvarPop tasks 
                                >>= \x -> return $ Just x
                           else return Nothing

parRun :: TaskRunnerStrategy -> Int -> [IO ()] -> IO ()
parRun s poolSize tasks = do
    t <- atomically $ newTVar tasks
    terminators <- replicateM poolSize $ do
            terminator <- atomically $ newTVar False
            myForkIO (taskRunner s terminator t)
            return terminator
    atomically $ do
        terminated <- mapM readTVar terminators >>= return.and --  (liftT (return.null)) t
        check terminated


parRunWithMonitor :: TaskRunnerStrategy -> Int -> (TVar [IO ()] -> [TVar Bool] -> IO ()) -> [IO ()] -> IO ()

parRunWithMonitor s poolSize monitor tasks = do
    t <- atomically $ newTVar tasks
    terminators <- replicateM poolSize $ do
            terminator <- atomically $ newTVar False
            myForkIO (taskRunner s terminator t)
            return terminator
    forkIO $ (monitor t terminators)
    atomically $ do
        terminated <- mapM readTVar terminators >>= return.and --  (liftT (return.null)) t
        check terminated

--startPool :: Int -> [IO ()] -> IO ()
--startPool poolSize tasks = do
--    t <- atomically $ newTVar tasks
--    pool <- replicateM poolSize (myForkIO (tasks t))
--    atomically $ do
--        terminated <- (liftT (return.null)) t
--        check terminated
