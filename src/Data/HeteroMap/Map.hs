{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances, TypeOperators, GADTs #-}

module Data.HeteroMap.Map
    ( Key, newKey
    , Map, empty, singleton, insert, lookup, update
    )
where

import Prelude hiding (lookup)

class In x xs where
    access :: xs -> x
    replace :: x -> xs -> xs

instance In x (x :* xs) where
    access (a :* _) = a
    replace x (_ :* as) = (x :* as)

instance In x xs => In x (y :* xs) where
    access (_ :* as) = access as
    replace x (a :* as) = (a :* replace x as)

data Z = Z
data a :* b = a :* !b

-- | A Key in a heterogeneous map. @x@ is the key identifier type,
-- which ensures that we don't look up a key in a map that doesn't
-- have it.
data Key x a where
    Key :: Key a a

-- | A heterogeneous map, including keys @xs@.
newtype Map xs = Map xs

-- | Creates an empty map.
empty :: Map Z
empty = Map Z

-- | Creates a map containing 1 key-value pair.
singleton :: Key x a -> a -> Map (x :* Z)
singleton Key v = Map (v :* Z)

-- | Allocate a new key, which is only valid within the passed function
-- (and will be a type error if it tries to escape).
newKey :: (forall x. Key x a -> b) -> b
newKey cc = cc Key

-- | Inserts a value into the map.
insert :: Key x a -> a -> Map xs -> Map (x :* xs)
insert Key val (Map m) = Map (val :* m)

-- | Looks up a value inside the map.
lookup :: In x xs => Key x a -> Map xs -> a
lookup Key (Map m) = access m

-- | Same as insert, except does not add the key identifier to the map,
-- instead ensuring that it's already there.
update :: In x xs => Key x a -> a -> Map xs -> Map xs
update Key x (Map m) = Map (replace x m)
