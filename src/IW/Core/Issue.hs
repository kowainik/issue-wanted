{-# LANGUAGE DeriveAnyClass #-}

module IW.Core.Issue
       ( Issue (..)
       , Label (..)
       , issueUrl
       ) where

import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.Url (Url (..))


-- | Wrapper for issue label names.
newtype Label = Label { unLabel :: Text }
    deriving stock   (Generic, Show)
    deriving newtype (Eq, Ord, FromField, ToField, FromJSON, ToJSON, Elm, FromHttpApiData)

-- | Data type representing a GitHub issue.
data Issue = Issue
    { issueRepoOwner :: !RepoOwner
    , issueRepoName  :: !RepoName
    , issueNumber    :: !Int
    , issueTitle     :: !Text
    , issueBody      :: !Text
    , issueLabels    :: !(SqlArray Label)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromRow, ToRow)
      deriving (Elm, ToJSON) via ElmStreet Issue

issueUrl :: Issue -> Url
issueUrl Issue{..} = Url
    $ "https://github.com/"
    <> unRepoOwner issueRepoOwner
    <> "/"
    <> unRepoName issueRepoName
    <> "/issues/"
    <> show issueNumber
