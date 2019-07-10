module IW.Core.SqlArray
    ( SqlArray (..)
    ) where

import Database.PostgreSQL.Simple.Types (PGArray (..))


newtype SqlArray a = SqlArray { unSqlArray :: [a] }
    deriving stock   (Generic)
    deriving newtype (Eq, ToJSON, Show)
    deriving (ToField, FromField) via PGArray a
