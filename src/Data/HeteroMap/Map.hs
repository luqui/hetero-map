{-# LANGUAGE IncoherentInstances, RankNTypes, MultiParamTypeClasses, FlexibleInstances, TypeOperators, GADTs #-}

module Data.HeteroMap.Map
    ( Key, newKey
    , Map, empty, singleton, insert, lookup, overwrite
    )
where

import Prelude hiding (lookup)
import Data.Unique
import GHC.Prim (Any)
import qualified Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe (fromJust)

class In x xs where 
    access :: xs -> x
    replace :: x -> xs -> xs
instance In x (x :* xs) where 
    access (a:*as) = a
    replace x (a:*as) = (x:*as)
instance In x xs => In x (y:*xs) where
    access (a:*as) = access as
    replace x (a:*as) = (a:*replace x as)

data Z = Z
data a :* b = a :* !b

-- | A Key in a heterogeneous map.  @x@ is the key identifier type, 
-- which ensures that we don't look up a key in a map that doesn't
-- have it.
data Key x a where
    Key :: Key a a

-- | A heterogeneous map, including keys @xs@.
newtype Map xs = Map xs

empty :: Map Z
empty = Map Z

singleton :: Key x a -> a -> Map (x :* Z)
singleton Key v = Map (v :* Z)

-- | Allocate a new key, which is only valid within the passed function
-- (and will be a type error if it tries to escape).
newKey :: (forall x. Key x a -> b) -> b
newKey cc = cc Key

insert :: Key x a -> a -> Map xs -> Map (x :* xs)
insert Key val (Map m) = Map (val :* m)

lookup :: In x xs => Key x a -> Map xs -> a
lookup Key (Map m) = access m

-- | Same as insert, except does not add the key identifier to the map,
-- instead ensuring that it's already there.
overwrite :: In x xs => Key x a -> a -> Map xs -> Map xs
overwrite Key x (Map m) = Map (replace x m)
