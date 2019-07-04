module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv)
import IW.Core.Id (Id (..))
import IW.Core.Issue (Issue (..))
import IW.Core.Repo (RepoName (..), RepoOwner (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Db (getIssues, prepareDb, upsertIssues)
import IW.Effects.Log (runAppLogIO_)

import Test.Assert (equals, equals_)
import Test.Common
import Test.Hspec (Spec, before_, describe, it)


dbSpecs :: AppEnv -> Spec
dbSpecs = joinSpecs "DB"
    [ getIssuesSpec
    , upsertIssuesSpec
    ]

getIssuesSpec :: AppEnv -> Spec
getIssuesSpec env = describe "getIssues function" $ do
    it "should return a list of length 1" $
        env & (length <$> getIssues) `equals` 1

-- When repo exists and issue doesn't exist, it should be inserted.
-- When repo exists and issue exists, it should be updated.

upsertIssuesSpec :: AppEnv -> Spec
upsertIssuesSpec env = before_ (runAppLogIO_ env prepareDb) $ do 
    describe "upsertIssues function" $ do
        it "should leave the issues table unaffected" $
            env & ( do before <- getIssues
                       upsertIssues [invalidIssue]
                       return before    
                  ) `equals_` getIssues 
        it "should insert an issue if its repo exists" $
            env & ( do before <- getIssues
                       upsertIssues [validIssue]
                       return $ length before
                  ) `equals_` (pred <$> length <$> getIssues)
  where
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
        { issueId        = Id 2
        , issueRepoOwner = RepoOwner "owner123"
        , issueRepoName  = RepoName "repo123"
        , issueNumber    = 321
        , issueTitle     = "Another test issue"
        , issueBody      = "Just another test issue"
        , issueLabels    = SqlArray ["help wanted"]
        }
