module Test.Data where

import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))


invalidIssue :: Issue
invalidIssue = Issue 
    { issueId        = Id 1
    , issueRepoOwner = RepoOwner "robert"
    , issueRepoName  = RepoName "instajam"
    , issueNumber    = 1
    , issueTitle     = "Not an issue"
    , issueBody      = ""
    , issueLabels    = SqlArray ["help wanted"]
    }

validIssue :: Issue
validIssue = Issue 
    { issueId        = Id 1
    , issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Another test issue"
    , issueBody      = "Just another test issue"
    , issueLabels    = SqlArray ["help wanted"]
    }

updateIssue :: Issue
updateIssue = Issue 
    { issueId        = Id 1
    , issueRepoOwner = RepoOwner "owner123"
    , issueRepoName  = RepoName "repo123"
    , issueNumber    = 123
    , issueTitle     = "Update test issue"
    , issueBody      = "Updated test issue body"
    , issueLabels    = SqlArray ["low hanging fruit"]
    }
