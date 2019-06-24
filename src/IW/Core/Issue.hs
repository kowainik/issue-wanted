{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       ) where
  
import IW.Core.Id (Id (..))
import IW.Core.Repo (Repo)


-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueId     :: Id Issue
    , issueNumber :: Int
    , issueTitle  :: Text
    , issueBody   :: Text
    , issueUrl    :: Text
    , issueRepoId :: Id Repo
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromJSON, ToJSON, FromRow, ToRow)
