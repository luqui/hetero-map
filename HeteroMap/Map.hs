{-# LANGUAGE IncoherentInstances, RankNTypes, TypeFamilies, MultiParamTypeClasses, FlexibleInstances #-}

module HeteroMap.Map
    ( Key, newKey
    , Map, empty, singleton, insert, lookup, overwrite, union
    )
where

import Prelude hiding (lookup)
import Data.Unique
import GHC.Prim (Any)
import qualified Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)

class In x xs
instance In x (x,xs)
instance In x xs => In x (y,xs)

-- | A Key in a heterogeneous map.  @x@ is the key identifier type, 
-- which ensures that we don't look up a key in a map that doesn't
-- have it.
newtype Key x a = Key Unique

-- | A heterogeneous map, including keys @xs@.
newtype Map xs = Map (Map.Map Unique Any)

empty :: Map ()
empty = Map Map.empty

singleton :: Key x a -> a -> Map (x,())
singleton k v = insert k v empty

-- | Allocate a new key, which is only valid within the passed function
-- (and will be a type error if it tries to escape).
newKey :: (forall x. Key x a -> b) -> b
newKey cc = cc (Key key)
    where
    {-# NOINLINE key #-}
    key = unsafePerformIO newUnique

insert :: Key x a -> a -> Map xs -> Map (x,xs)
insert (Key key) val (Map m) = Map (Map.insert key (unsafeCoerce val) m)

lookup :: In x xs => Key x a -> Map xs -> a
lookup (Key refid) (Map m) = unsafeCoerce . fromJust $ Map.lookup refid m

-- | Same as insert, except does not add the key identifier to the map,
-- instead ensuring that it's already there.
overwrite :: In x xs => Key x a -> a -> Map xs -> Map xs
overwrite (Key refid) x (Map m) = Map (Map.insert refid (unsafeCoerce x) m)

type family Union xs ys
type instance Union () ys = ys
type instance Union (x,xs) ys = (x, Union xs ys)

union :: Map xs -> Map ys -> Map (Union xs ys)
union (Map m) (Map m') = Map (Map.union m m')
