module Utils.BinaryInstances where

import Data.Binary

import Utils.Rectangle

instance (Binary a) => Binary (Rectangle a) where
    put (Rectangle a) = put a
    get = get >>= return.Rectangle
