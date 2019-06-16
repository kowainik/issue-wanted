{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       ) where

-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueId     :: Int
    , issueNumber :: Int
    , issueTitle  :: Text
    , issueBody   :: Text
    , issueUrl    :: Text
    , issueRepoId :: Int
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON) 
