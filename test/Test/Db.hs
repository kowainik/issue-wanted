module Test.Db 
       ( dbSpecs
       ) where

import IW.App (AppEnv)
import IW.Db (getIssues, upsertIssues)

import Test.Assert (equals, equals_)
import Test.Data (invalidIssue, updateIssue, validIssue)
import Test.Hspec (Spec, describe, it, pending)


dbSpecs :: AppEnv -> Spec
dbSpecs env = describe "Databse SQL query correctness" $ do
    describe "Repo" $ do
        describe "upsertRepos" $ do
            it "should leave the repos table unaffected" $
                pending 
            it "should update the repo if the same repo exists" $
                pending
        describe "getRepos" $ do
            it "should return a list of issues with length 2" $
                pending
    describe "Issue" $ do
        describe "upsertIssues" $ do
            it "should insert an issue if its repo exists" $
                env & ( do before <- getIssues
                           upsertIssues [validIssue]
                           return $ length before
                  ) `equals_` (prec . length <$> getIssues)
            it "should leave the issues table unaffected" $
                env & ( do before <- getIssues
                           upsertIssues [invalidIssue]
                           return before    
                      ) `equals_` getIssues 
            it "should update the issue if the same issue exists" $
                env & ( do before <- getIssues
                           upsertIssues [updateIssue]
                           return $ length before
                      ) `equals_` (length <$> getIssues)
        describe "getIssues" $ do
            it "should return a list of issues with length 1" $
                env & (length <$> getIssues) `equals` 1
