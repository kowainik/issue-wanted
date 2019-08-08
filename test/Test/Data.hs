module Test.Data
       ( -- * Issue
         invalidIssue
       , validIssue
       , updatedValidIssue

         -- * Repo
       , validRepo
       , updatedValidRepo
       ) where

import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoName (..), RepoOwner (..), Category (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Sync.Search (mkRepoCabalUrl)


validRepo :: Repo
validRepo = Repo
    { repoOwner      = RepoOwner "owner123"
    , repoName       = RepoName "repo123"
    , repoDescr      = "A test repo."
    , repoCategories = SqlArray [Category "FFI"]
    , repoCabalUrl   = mkRepoCabalUrl (RepoOwner "owner123") (RepoName "repo123") Nothing
    }

updatedValidRepo :: Repo
updatedValidRepo = Repo
    { repoOwner      = RepoOwner "owner123"
    , repoName       = RepoName "repo123"
    , repoDescr      = "Updating test repo description."
    , repoCategories = SqlArray
        [ Category "Testing"
        , Category "FFI"
        ]
    , repoCabalUrl   = mkRepoCabalUrl (RepoOwner "owner123") (RepoName "repo123") Nothing
    }

invalidIssue :: Issue
invalidIssue = Issue
    { issueRepoOwner = RepoOwner "robert"
    , issueRepoName  = RepoName "instajam"
    , issueNumber    = 1
    , issueTitle     = "Not an issue"
    , issueBody      = ""
    , issueLabels    = SqlArray [Label "help wanted"]
    }

validIssue :: Issue
validIssue = Issue
    { issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Another test issue"
    , issueBody      = "Just another test issue"
    , issueLabels    = SqlArray [Label "help wanted"]
    }

updatedValidIssue :: Issue
updatedValidIssue = Issue
    { issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Update test issue"
    , issueBody      = "Updated test issue body"
    , issueLabels    = SqlArray [Label "low hanging fruit", Label "good first issue"]
    }
