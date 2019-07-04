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
    , issueRepoOwner :: RepoOwner
    , issueRepoName  :: RepoName
    , issueNumber    :: Int
    , issueTitle     :: Text
    , issueBody      :: Text
    , issueLabels    :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow)

instance ToRow Issue where
    toRow Issue{..} = toRow (issueRepoOwner, issueRepoName, issueNumber, issueTitle, issueBody, issueLabels)

issueUrl :: Issue -> Text
issueUrl Issue{..} = 
    "https://github.com/" 
    <> unRepoOwner issueRepoOwner 
    <> "/" 
    <> unRepoName issueRepoName 
    <> "/issues/" 
    <> show issueNumber
    