{-# LANGUAGE DeriveAnyClass #-}

{- | @WithId a@ provides an @Id a@ field to types that don't have one. This was added
because types with an @Id a@ can be sorted and updated more effeciently.
-}

module IW.Core.WithId
       ( WithId (..)
       ) where

import IW.Core.Id (Id (..))

import Database.PostgreSQL.Simple.FromRow (RowParser)
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.Types ((:.) (..))


data WithId a = WithId
    { withIdId  :: !(Id a)
    , withIdVal :: !a
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON)

instance FromRow a => FromRow (WithId a) where
    fromRow :: RowParser (WithId a)
    fromRow = WithId <$> field <*> fromRow
    {-# INLINE fromRow #-}

instance ToRow a => ToRow (WithId a) where
    toRow :: WithId a -> [Action]
    toRow WithId{..} = toRow (Only withIdId :. withIdVal)
    {-# INLINE toRow #-}
