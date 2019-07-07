{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       , Label (..)
       , issueUrl
       ) where
  
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))


-- | Wrapper for issue label names
newtype Label = Label { unLabel :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, FromHttpApiData)

-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueRepoOwner :: !RepoOwner
    , issueRepoName  :: !RepoName
    , issueNumber    :: !Int
    , issueTitle     :: !Text
    , issueBody      :: !Text
    , issueLabels    :: !(SqlArray Label)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (ToJSON, FromRow, ToRow)

issueUrl :: Issue -> Text
issueUrl Issue{..} = 
    "https://github.com/" 
    <> unRepoOwner issueRepoOwner 
    <> "/" 
    <> unRepoName issueRepoName 
    <> "/issues/" 
    <> show issueNumber
    