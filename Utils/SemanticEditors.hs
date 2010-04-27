module Utils.SemanticEditors where
import Data.Array
import qualified Data.Map as Map
import Control.Arrow
-- Semantic editors, maybe to move somewhere else

 -- Array element
element i = \e arr -> arr // [(i,e (arr ! i))]
 
-- Finite Map 
value k = \e map -> Map.adjust e k map

-- Lists of pairs -- Not a combinator
inzipmap sel fs = uncurry zip . sel (fmap fs)  . unzip
inzip sel fs = uncurry zip . sel (fs)  . unzip


