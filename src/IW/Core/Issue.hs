{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       ) where
  
import IW.Core.Id (Id (..))
import IW.Core.SqlArray (SqlArray (..))


-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueId       :: Id Issue
    , issueNumber   :: Int
    , issueTitle    :: Text
    , issueBody     :: Text
    , issueUrl      :: Text
    , issueOwner    :: Text
    , issueRepoName :: Text
    , issueLabels   :: SqlArray Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)
