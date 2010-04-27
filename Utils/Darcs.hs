{-# OPTIONS_GHC -XTemplateHaskell #-}
module Utils.Darcs where
import System.Process
import System.Exit
import Data.Maybe
import Language.Haskell.TH

getLastDarcsTag = do
                    (exit,output,_) <- readProcessWithExitCode "darcs" ["show","tags"] [] 
                    case exit of
                        ExitSuccess -> return . Just . head . lines $ output
                        _ -> return Nothing

thVersionTag =  runIO getLastDarcsTag>>=litE . StringL . fromMaybe "Non versioned system"
