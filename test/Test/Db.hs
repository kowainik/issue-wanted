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
            it "should do nothing when inserting an empty list" $
                env & succeeds (upsertIssues [])
            it "should insert an issue if its repo exists" $
                env & issuesAffectedRows (upsertIssues [validIssue]) `equals` 1
            it "should leave the issues table unaffected when there's no corresponding repo" $
                env & issuesUnaffected (upsertIssues [invalidIssue]) `equals` True 
            it "should update the issue if the same issue exists" $
                env & issuesAffectedRows (upsertIssues [updateIssue]) `equals` 0
        describe "getIssues" $
            it "should return a list of issues with length 1" $
                env & issuesRows `equals` 1
  where
    -- | Returns number of rows currently in the issues database
    issuesRows :: App Int
    issuesRows = length <$> getIssues

    -- | Returns the number of affected rows in the issue table after an action
    issuesAffectedRows :: App () -> App Int
    issuesAffectedRows action = do
        before <- issuesRows   
        action
        after <- issuesRows
        pure $ after - before

    -- | Returns True if no rows in the issues table were affected after an action
    issuesUnaffected :: App () -> App Bool
    issuesUnaffected action = do
        before <- issuesRows 
        action
        after <- issuesRows
        pure $ after == before
