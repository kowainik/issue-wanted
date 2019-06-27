{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.PGArray'
    ( PGArray' (..)
    ) where

import Database.PostgreSQL.Simple.Types (PGArray (..))
import Database.PostgreSQL.Simple.FromField (FieldParser, pgArrayFieldParser)
import Database.PostgreSQL.Simple.ToField (Action (..))


newtype PGArray' a = PGArray' { fromPGArray' :: [a] }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJSON)

instance (FromField a, Typeable a) => FromField (PGArray' a) where
    fromField :: FieldParser (PGArray' a)
    fromField f dat = PGArray' . fromPGArray <$> pgArrayFieldParser fromField f dat

instance (ToField a) => ToField (PGArray' a) where
    toField :: PGArray' a -> Action
    toField = toField . PGArray . fromPGArray' 
