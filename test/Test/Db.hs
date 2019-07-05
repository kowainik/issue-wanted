module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv, App)
import IW.Core.Issue (Issue)
import IW.Db (getIssues, upsertIssues)

import Test.Assert (equals, succeeds)
import Test.Common (joinSpecs)
import Test.Data (invalidIssue, updateIssue, validIssue)
import Test.Hspec (Spec, describe, it, pending)


dbSpecs :: AppEnv -> Spec
dbSpecs = joinSpecs "Databse SQL query correctness"
    [ repoSpecs
    , issueSpecs
    ]

-- | Takes a getter function for getting values from the environment before and after an action, 
-- a function for comparing those values, and the action
beforeAfter :: App a -> (a -> a -> b) -> App c -> App b
beforeAfter getter comparison action = do
    before <- getter 
    _  <- action
    after <- getter
    pure $ after `comparison` before

---------------
-- REPO SPEC --
---------------

repoSpecs :: AppEnv -> Spec
repoSpecs = joinSpecs "Repo"
    [ upsertReposSpec
    , getReposSpec 
    ]

upsertReposSpec :: AppEnv -> Spec
upsertReposSpec env = describe "upsertRepos" $ do
    it "should leave the repos table unaffected" 
        pending 
    it "should update the repo if the same repo exists"
        pending

getReposSpec :: AppEnv -> Spec
getReposSpec env = describe "getRepos" $
    it "should return a list of repos with length 1"
        pending

----------------
-- ISSUE SPEC --
----------------

issueSpecs :: AppEnv -> Spec
issueSpecs = joinSpecs "Issue"
     [ upsertIssuesSpec
     , getIssuesSpec
     ]

upsertIssuesSpec :: AppEnv -> Spec
upsertIssuesSpec env = describe "upsertIssues" $ do
    it "should do nothing when inserting an empty list" $ do
        env & succeeds (upsertIssues [])
        env & issuesAffectedRows (upsertIssues []) `equals` 0
    it "should insert issues if its repo exists" $ do
        env & issuesIncreased (upsertIssues [validIssue]) `equals` True
    it "should leave the issues table unaffected when there's no corresponding repo" $
        env & issuesUnaffected (upsertIssues [invalidIssue]) `equals` True 
    it "should update the issue if the same issue exists" $
        env & issuesRowDifference (upsertIssues [updateIssue]) `equals` [updateIssue]

getIssuesSpec :: AppEnv -> Spec
getIssuesSpec env = describe "getIssues" $
    it "should return a list of issues with length 1" $ do
        env & issuesRowCount `equals` 1
        env & issuesUnaffected (getIssues) `equals` True 

-- | Returns number of rows currently in the issues database
issuesRowCount :: App Int
issuesRowCount = length <$> getIssues

-- | Returns the number of affected rows in the issue table after an action
issuesAffectedRows :: App a -> App Int
issuesAffectedRows action = beforeAfter issuesRowCount (-) action 

-- | Returns True if no rows in the issues table were affected after an action
issuesUnaffected :: App a -> App Bool
issuesUnaffected action = beforeAfter getIssues (==) action

-- | Returns True if number of rows in the issues table increased after an action
issuesIncreased :: App a -> App Bool
issuesIncreased action = beforeAfter issuesRowCount (>) action 

-- | Returns difference between rows of the issue table after and before an action
issuesRowDifference :: App a -> App [Issue]
issuesRowDifference action = beforeAfter getIssues (\\) action
