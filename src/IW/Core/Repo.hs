{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Repo
       ( Repo (..)
       , RepoOwner (..)
       , RepoName (..)
       , Category (..)
       , repoUrl
       ) where

import IW.Core.SqlArray (SqlArray (..))


-- | Wrapper for repository owner
newtype RepoOwner = RepoOwner { unRepoOwner :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData)

-- | Wrapper for repository name
newtype RepoName = RepoName { unRepoName :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData)

-- | Wrapper for repository Hackage category names
newtype Category = Category { unCategory :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, ToJSON, FromHttpApiData)

-- | Data type representing a GitHub repository
data Repo = Repo 
    { repoOwner      :: !RepoOwner
    , repoName       :: !RepoName
    , repoDescr      :: !Text
    , repoCategories :: !(SqlArray Category)
    } deriving stock    (Eq, Generic, Show)
      deriving anyclass (ToJSON, FromRow, ToRow)

repoUrl :: Repo -> Text
repoUrl Repo{..} = 
    "https://github.com/" 
    <> unRepoOwner repoOwner 
    <> "/" 
    <> unRepoName repoName 
