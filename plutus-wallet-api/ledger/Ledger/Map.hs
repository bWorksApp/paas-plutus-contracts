{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- Prevent unboxing, which the plugin can't deal with
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
-- A map implementation that can be used in on-chain and off-chain code.
module Ledger.Map(
    Map
    , IsEqual
    , singleton
    , empty
    , fromList
    , toList
    , keys
    , map
    , lookup
    , union
    , all
    -- * These
    , These(..)
    , these
    ) where

import           Codec.Serialise.Class        (Serialise)
import           Data.Aeson                   (FromJSON (parseJSON), ToJSON (toJSON))
import           Data.Hashable                (Hashable)
import           Data.Swagger.Internal.Schema (ToSchema)
import           GHC.Generics                 (Generic)
import           Language.PlutusTx.Lift       (makeLift)
import           Language.PlutusTx.Prelude    hiding (all, lookup, map)
import qualified Language.PlutusTx.Prelude    as P

import           Ledger.These

{-# ANN module ("HLint: ignore Use newtype instead of data"::String) #-}

-- | A 'Map' of key-value pairs.
data Map k v = Map { unMap :: [(k, v)] }
    deriving (Show)
    deriving stock (Generic)
    deriving anyclass (ToSchema, Serialise, Hashable)

makeLift ''Map

instance (ToJSON v, ToJSON k) => ToJSON (Map k v) where
    toJSON = toJSON . unMap

instance (FromJSON v, FromJSON k) => FromJSON (Map k v) where
    parseJSON v = Map <$> parseJSON v

fromList :: [(k, v)] -> Map k v
fromList = Map

toList :: Map k v -> [(k, v)]
toList (Map l) = l

-- | Apply a function to the values of a 'Map'.
map :: forall k v w . (v -> w) -> Map k v -> Map k w
map f (Map mp) =
    let
        go :: [(k, v)] -> [(k, w)]
        go []           = []
        go ((c, i):xs') = (c, f i) : go xs'
    in Map (go mp)

-- | Compare two 'k's for equality.
type IsEqual k = k -> k -> Bool

-- | Find an entry in a 'Map'.
lookup :: forall k v . IsEqual k -> k -> Map k v -> Maybe v
lookup eq c (Map xs) =
    let
        go :: [(k, v)] -> Maybe v
        go []            = Nothing
        go ((c', i):xs') = if eq c' c then Just i else go xs'
    in go xs

-- | The keys of a 'Map'.
keys :: Map k v -> [k]
keys (Map xs) = P.map (\(k, _ :: v) -> k) xs

-- | Combine two 'Map's.
union :: forall k v r . IsEqual k -> Map k v -> Map k r -> Map k (These v r)
union eq (Map ls) (Map rs) =
    let
        f :: v -> Maybe r -> These v r
        f a b' = case b' of
            Nothing -> This a
            Just b  -> These a b

        ls' :: [(k, These v r)]
        ls' = P.map (\(c, i) -> (c, f i (lookup eq c (Map rs)))) ls

        rs' :: [(k, r)]
        rs' = filter (\(c, _) -> not (any (\(c', _) -> eq c' c) ls)) rs

        rs'' :: [(k, These v r)]
        rs'' = P.map (\(c, b) -> (c, That b)) rs'

    in Map (append ls' rs'')

-- | See 'Data.Map.all'
all :: (v -> Bool) -> Map k v -> Bool
all p (Map mps) =
    let go xs = case xs of
            []              -> True
            (_ :: k, x):xs' -> p x && go xs'
    in go mps

-- | A singleton map.
singleton :: k -> v -> Map k v
singleton c i = Map [(c, i)]

-- This has to take unit otherwise it falls foul of the value restriction. Moreover,
-- we need to mark it INLINABLE, since the optimized unfolding we get from `-fexpose-all-unfoldings`
-- ignores the unit, breaking our workaround.
{-# INLINABLE empty #-}
-- | An empty 'Map'.
empty :: () -> Map k v
empty _ = Map ([] :: [(k, v)])
