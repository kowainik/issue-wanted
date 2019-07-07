module IW.Core.SqlArray
    ( SqlArray (..)
    ) where

import Database.PostgreSQL.Simple.Types (PGArray (..))


newtype SqlArray a = SqlArray { unSqlArray :: [a] }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, ToJSON)
    deriving (ToField, FromField) via PGArray a
