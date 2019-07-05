module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Db (getIssues, upsertIssues)

import Test.Assert (equals, equals_)
import Test.Hspec (Spec, describe, it, pending)


dbSpecs :: AppEnv -> Spec
dbSpecs env = describe "Databse SQL query correctness" $ do
    describe "Issue" $ do
        describe "upsertIssues" $ do
            it "should leave the issues table unaffected" $
                env & ( do before <- getIssues
                           upsertIssues [invalidIssue]
                           return before    
                      ) `equals_` getIssues 
            it "should insert an issue if its repo exists" $
                env & ( do before <- getIssues
                           upsertIssues [validIssue]
                           return $ length before
                  ) `equals_` (prec . length <$> getIssues)
            it "should update the issue if the same issue exists" $
                env & ( do before <- getIssues
                           upsertIssues [updateIssue]
                           return $ length before
                      ) `equals_` (length <$> getIssues)
        describe "getIssues" $ do
            it "should return a list of issues with length 2" $
                env & (length <$> getIssues) `equals` 2 
    describe "Repo" $ do
        describe "upsertRepos" $ do
            it "should leave the repos table unaffected" $
                pending 
            it "should update the repo if the same repo exists" $
                pending
        describe "getRepos" $ do
            it "should return a list of issues with length 2" $
                pending
    
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
