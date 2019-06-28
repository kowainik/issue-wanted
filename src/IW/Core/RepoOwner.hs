{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module IW.Core.RepoOwner
       ( RepoOwner (..)
       ) where


-- | Wrapper for repository owner.
newtype RepoOwner = RepoOwner { unRepoOwner :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)
