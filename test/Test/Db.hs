module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Db.Issue (getIssues, upsertIssues)

import Test.Assert (equals, equals_)
import Test.Common
import Test.Hspec (Spec, describe, it)


dbSpecs :: AppEnv -> Spec
dbSpecs = joinSpecs "DB"
    [ getIssuesSpec
    , upsertIssuesSpec
    ]

getIssuesSpec :: AppEnv -> Spec
getIssuesSpec env = describe "getIssues function" $ do
    it "should return a list of length 1" $
        env & (length <$> getIssues) `equals` 1

upsertIssuesSpec :: AppEnv -> Spec
upsertIssuesSpec env = describe "upsertIssues function" $ do
    it "should leave the issues table unaffected" $
        env & ( do
                  before <- getIssues
                  upsertIssues [dummyIssue]
                  return before    
              ) `equals_` getIssues 
  where
    dummyIssue :: Issue
    dummyIssue = Issue 
        { issueId        = Id 1
        , issueRepoOwner = RepoOwner "robert"
        , issueRepoName  = RepoName "instajam"
        , issueNumber    = 1
        , issueTitle     = "Not an issue"
        , issueBody      = ""
        , issueLabels    = SqlArray ["help wanted"]
        }
