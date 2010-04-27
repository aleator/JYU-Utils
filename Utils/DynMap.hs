module Utils.DynMap where
import qualified Data.Map as Map
import Data.Dynamic
import Control.Monad.State
import Control.Monad
import Data.List(intercalate)

newtype PropertyMap = PM (Map.Map String (String,Dynamic))

emptyD :: PropertyMap
emptyD = PM Map.empty
insertD k v (PM dmap) = PM $ Map.insert k (show v,toDyn v) dmap
(PM dmap) #? k   = fromDyn (snd $ dmap Map.! k) (error $ "TYPE ERROR ON "++k)
(PM dmap) #?? k  = (fst $ dmap Map.! k) 
(PM dmap) #/ k  = (Map.delete k dmap) 

showD dmap = "{" ++ (intercalate ", "  . map (\(k,(v,p)) -> k++": "++v) . Map.toList $ dmap) ++"}"

instance Show PropertyMap where
    show (PM pm) = "{"++ 
                    (intercalate ", " . map (\(k,(v,_))-> k++":" ++v)  $ Map.assocs pm)
                   ++"}"

(=:) :: (Show a,Typeable a) => String -> a -> State PropertyMap ()
a =: b = do 
        x <- get
        put (insertD a b x)
          
properties st = snd $ runState st emptyD


