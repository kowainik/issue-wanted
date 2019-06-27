{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       ) where
  
import IW.Core.Id (Id (..))
import IW.Core.PGArray' (PGArray' (..))


-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueId       :: Id Issue
    , issueNumber   :: Int
    , issueTitle    :: Text
    , issueBody     :: Text
    , issueUrl      :: Text
    , issueOwner    :: Text
    , issueRepoName :: Text
    , issueLabels   :: PGArray' Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (ToJSON, FromRow, ToRow)
