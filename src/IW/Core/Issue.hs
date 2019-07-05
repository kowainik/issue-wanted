{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       , issueUrl
       ) where
  
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))


-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueRepoOwner :: !RepoOwner
    , issueRepoName  :: !RepoName
    , issueNumber    :: !Int
    , issueTitle     :: !Text
    , issueBody      :: !Text
    , issueLabels    :: !(SqlArray Text)
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)

issueUrl :: Issue -> Text
issueUrl Issue{..} = 
    "https://github.com/" 
    <> unRepoOwner issueRepoOwner 
    <> "/" 
    <> unRepoName issueRepoName 
    <> "/issues/" 
    <> show issueNumber
    