module Utils.Exception where
import Control.Exception as E
import System.Exit

inProcess a b = tagEM b a 
tagEM op msg = op 
              `E.catch` 
               (\e -> error ("Error on "++msg
                                           ++" ("
                                           ++show (e::SomeException)
                                           ++")"))

tagE op msg = mapException tag op
    where 
        tag :: SomeException -> ErrorCall
        tag e = ErrorCall $ "Error in "++msg++" ("++show e++")"

onErrorTerminateWith op msg = op 
                               `E.catch` 
                               (\e -> putStrLn ("Error on "++msg
                                                           ++" ("
                                                           ++show (e::SomeException)
                                                           ++")") 
                                      >> exitWith (ExitFailure 1)) 
