{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.SqlArray
    ( SqlArray (..)
    ) where

import Database.PostgreSQL.Simple.Types (PGArray (..))
import Database.PostgreSQL.Simple.FromField (FieldParser, pgArrayFieldParser)
import Database.PostgreSQL.Simple.ToField (Action (..))


newtype SqlArray a = SqlArray { unSqlArray :: [a] }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJSON)

instance (FromField a, Typeable a) => FromField (SqlArray a) where
    fromField :: FieldParser (SqlArray a)
    fromField f dat = SqlArray . fromPGArray <$> pgArrayFieldParser fromField f dat

instance (ToField a) => ToField (SqlArray a) where
    toField :: SqlArray a -> Action
    toField = toField . PGArray . unSqlArray 
