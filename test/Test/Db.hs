module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv, App)
import IW.Db (getIssues, upsertIssues)

import Test.Assert (equals, succeeds)
import Test.Data (invalidIssue, updateIssue, validIssue)
import Test.Hspec (Spec, describe, it, pending)


dbSpecs :: AppEnv -> Spec
dbSpecs env = describe "Databse SQL query correctness" $ do
    describe "Repo" $ do
        describe "upsertRepos" $ do
            it "should leave the repos table unaffected" 
                pending 
            it "should update the repo if the same repo exists"
                pending
        describe "getRepos" $
            it "should return a list of issues with length 2"
                pending
    describe "Issue" $ do
        describe "upsertIssues" $ do
            it "should do nothing when inserting an empty list" $ do
                env & succeeds (upsertIssues [])
                env & issuesAffectedRows (upsertIssues []) `equals` 0
            it "should insert an issue if its repo exists" $ do
                env & issuesAffectedRows (upsertIssues [validIssue]) `equals` 1
            it "should leave the issues table unaffected when there's no corresponding repo" $
                env & issuesUnaffected (upsertIssues [invalidIssue]) `equals` True 
            it "should update the issue if the same issue exists" $
                env & issuesAffectedRows (upsertIssues [updateIssue]) `equals` 0
        describe "getIssues" $
            it "should return a list of issues with length 1" $ do
                env & issuesRows `equals` 1
                env & issuesUnaffected (getIssues) `equals` True 

  where
    -- | Takes a getter function for getting values from the environment before and after an action, 
    -- a function for comparing those values, and the action
    beforeAfter :: App a -> (a -> a -> b) -> App c -> App b
    beforeAfter getter comparison action = do
        before <- getter 
        _  <- action
        after <- getter
        pure $ after `comparison` before

    -- | Returns number of rows currently in the issues database
    issuesRows :: App Int
    issuesRows = length <$> getIssues

    -- | Returns the number of affected rows in the issue table after an action
    issuesAffectedRows :: App a -> App Int
    issuesAffectedRows action = beforeAfter issuesRows (-) action 

    -- | Returns True if no rows in the issues table were affected after an action
    issuesUnaffected :: App a -> App Bool
    issuesUnaffected action = beforeAfter getIssues (==) action

    -- | Returns True if number of rows in issues table increases after an action
    issuesIncrease :: App a -> App Bool
    issuesIncrease action = beforeAfter issuesRows (>) action 
