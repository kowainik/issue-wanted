{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       ) where
  
import IW.Core.Id (Id (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))


-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueId        :: Id Issue
    , issueNumber    :: Int
    , issueTitle     :: Text
    , issueBody      :: Text
    , issueRepoOwner :: RepoOwner
    , issueRepoName  :: RepoName
    , issueUrl       :: Text
    , issueLabels    :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)
