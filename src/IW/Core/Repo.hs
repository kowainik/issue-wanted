{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Repo
       ( Repo (..)
       ) where

import IW.Core.Id (Id (..))
import IW.Core.RepoName (RepoName (..))
import IW.Core.RepoOwner (RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))
        

-- | Data type representing a GitHub repository
data Repo = Repo 
    { repoId         :: Id Repo
    , repoOwner      :: RepoOwner
    , repoName       :: RepoName
    , repoDescr      :: Text
    , repoCategories :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)
