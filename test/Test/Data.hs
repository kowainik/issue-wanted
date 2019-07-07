module Test.Data
       ( -- * Issue
         invalidIssue
       , validIssue
       , updatedValidIssue

         -- * Repo 
       , validRepo
       , updatedValidRepo
       ) where

import IW.Core.Issue (Issue (..))
import IW.Core.Repo (Repo (..), RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))


validRepo :: Repo
validRepo = Repo 
    { repoOwner      = RepoOwner "owner123"
    , repoName       = RepoName "repo123"
    , repoDescr      = "A test repo."
    , repoCategories = SqlArray ["Testing", "FFI"]
    }

updatedValidRepo :: Repo
updatedValidRepo = Repo 
    { repoOwner      = RepoOwner "owner123"
    , repoName       = RepoName "repo123"
    , repoDescr      = "Updating test repo description."
    , repoCategories = SqlArray ["Testing"]
    }

invalidIssue :: Issue
invalidIssue = Issue 
    { issueRepoOwner = RepoOwner "robert"
    , issueRepoName  = RepoName "instajam"
    , issueNumber    = 1
    , issueTitle     = "Not an issue"
    , issueBody      = ""
    , issueLabels    = SqlArray ["help wanted"]
    }

validIssue :: Issue
validIssue = Issue 
    { issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Another test issue"
    , issueBody      = "Just another test issue"
    , issueLabels    = SqlArray ["help wanted"]
    }

updatedValidIssue :: Issue
updatedValidIssue = Issue 
    { issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Update test issue"
    , issueBody      = "Updated test issue body"
    , issueLabels    = SqlArray ["low hanging fruit"]
    }
