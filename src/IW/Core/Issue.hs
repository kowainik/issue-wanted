{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       , issueUrl
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
    , issueLabels    :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow)

instance ToRow Issue where
    toRow Issue{..} = toRow (issueNumber, issueTitle, issueBody, issueRepoOwner, issueRepoName, issueLabels)

issueUrl :: Issue -> Text
issueUrl Issue{..} = 
    "https://github.com/" 
    <> unRepoOwner issueRepoOwner 
    <> "/" 
    <> unRepoName issueRepoName 
    <> "/issues/" 
    <> show issueNumber
