{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module IW.Core.Id
       ( Id (..)
       , AnyId
       , castId
       ) where

import Data.Type.Equality (type (==))


-- | Wrapper for integer id. Contains phantom type parameter for increased
-- type-safety.
newtype Id a = Id { unId :: Int }
    deriving stock (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)

-- | When we don't care about type of 'Id' but don't want to deal with type variables.
type AnyId = Id ()

-- | Unsafe cast of 'Id'. Implementation uses smart trick to enforce usage
-- always with @TypeApplications@.
castId :: forall to from to' . ((to == to') ~ 'True) => Id from -> Id to'
castId (Id a) = Id a
