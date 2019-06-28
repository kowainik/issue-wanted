{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}

module IW.Core.RepoName
       ( RepoName (..)
       ) where


-- | Wrapper for repository name.
newtype RepoName = RepoName { unRepoName :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)
