module IW.Core.SqlArray
    ( SqlArray (..)
    ) where

import Database.PostgreSQL.Simple.Types (PGArray (..))
import Database.PostgreSQL.Simple.ToField (Action)
import Database.PostgreSQL.Simple.FromField (FieldParser)


newtype SqlArray a = SqlArray { unSqlArray :: [a] }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, ToJSON)

instance ToField a => ToField (SqlArray a) where
    toField :: SqlArray a -> Action
    toField = coerce @(PGArray a -> Action) @(SqlArray a -> Action) toField

instance (FromField a, Typeable a) => FromField (SqlArray a) where
    fromField :: FieldParser (SqlArray a)
    fromField = coerce @(FieldParser (PGArray a)) @(FieldParser (SqlArray a)) fromField
