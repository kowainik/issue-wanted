{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Repo
       ( Repo (..)
       , RepoName (..)
       , RepoOwner (..)
       ) where

import IW.Core.Id (Id (..))
import IW.Core.SqlArray (SqlArray (..))
   

-- | Wrapper for repository name
newtype RepoName = RepoName { unRepoName :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)

-- | Wrapper for repository owner
newtype RepoOwner = RepoOwner { unRepoOwner :: Text }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)

-- | Data type representing a GitHub repository
data Repo = Repo 
    { repoId         :: Id Repo
    , repoOwner      :: RepoOwner
    , repoName       :: RepoName
    , repoDescr      :: Text
    , repoCategories :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)
