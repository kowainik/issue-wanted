{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Repo
       ( Repo (..)
       , RepoOwner (..)
       , RepoName (..)
       , Category (..)
       ) where

import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Url (Url)


-- | Wrapper for a repository owner.
newtype RepoOwner = RepoOwner { unRepoOwner :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, ToJSON, Elm, FromHttpApiData)

-- | Wrapper for a repository name.
newtype RepoName = RepoName { unRepoName :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, ToJSON, Elm, FromHttpApiData)

-- | Wrapper for repository Hackage category names.
newtype Category = Category { unCategory :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, Elm, FromHttpApiData)

-- | Data type representing a GitHub repository.
data Repo = Repo
    { repoOwner      :: !RepoOwner
    , repoName       :: !RepoName
    , repoDescr      :: !Text
    , repoCategories :: !(SqlArray Category)
    , repoCabalUrl   :: !Url
    } deriving stock    (Eq, Generic, Show)
      deriving anyclass (FromRow, ToRow)
      deriving (Elm, ToJSON) via ElmStreet Repo
