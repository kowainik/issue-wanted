module Test.Db
       ( dbSpecs
       ) where

import Data.List ((\\))
import Test.Hspec (Spec, describe, it)

import IW.App (AppEnv, App)
import IW.Core.Issue (Issue, Label (..))
import IW.Core.Id (Id (..))
import IW.Core.Repo (Repo, Category (..))
import IW.Core.WithId (WithId (..))
import IW.Db (getIssues, getRepos, getReposByCategories, getIssuesByLabels, upsertIssues, upsertRepos)
import Test.Assert (equals, succeeds)
import Test.Common (joinSpecs)
import Test.Data (invalidIssue, updatedValidIssue, updatedValidRepo, validIssue, validRepo)


dbSpecs :: AppEnv -> Spec
dbSpecs = joinSpecs "Databse SQL query correctness"
    [ repoSpecs
    , issueSpecs
    ]

-- | Takes a getter function for getting values from the environment before and after an action,
-- a function for comparing those values, and the action.
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
    , getReposByCategoriesSpec
    ]

upsertReposSpec :: AppEnv -> Spec
upsertReposSpec env = describe "upsertRepos" $ do
    it "should do nothing when inserting an empty list" $ do
        env & succeeds (upsertRepos [])
        env & reposAddedRows (upsertRepos []) `equals` 0
    it "should insert repos if the repos are valid" $
        env & reposIncreased (upsertRepos [validRepo]) `equals` True
    it "should update the repo if the same repo already exists" $
        env & reposRowDifference (upsertRepos [updatedValidRepo]) `equals` [Id 1 `WithId` updatedValidRepo]

getReposSpec :: AppEnv -> Spec
getReposSpec env = describe "getRepos" $
    it "should return a list of repos with length 1" $
        env & reposRowCount `equals` 1

getReposByCategoriesSpec :: AppEnv -> Spec
getReposByCategoriesSpec env = describe "getReposByCategories" $
    it "should return [Id 1 `WithId` updaterValidRepo]" $
        env & getReposByCategories [Category "FFI"] `equals` [Id 1 `WithId` updatedValidRepo]

-- | Returns number of rows currently in the repos database.
reposRowCount :: App Int
reposRowCount = length <$> getRepos

-- | Returns the number of rows added to the repo table after an action.
reposAddedRows :: App a -> App Int
reposAddedRows = beforeAfter reposRowCount (-)

-- | Returns True if number of rows in the repos table increased after an action.
reposIncreased :: App a -> App Bool
reposIncreased = beforeAfter reposRowCount (>)

-- | Returns difference between rows of the repos table after and before an action.
reposRowDifference :: App a -> App [WithId Repo]
reposRowDifference = beforeAfter getRepos (\\)

----------------
-- ISSUE SPEC --
----------------

issueSpecs :: AppEnv -> Spec
issueSpecs = joinSpecs "Issue"
     [ upsertIssuesSpec
     , getIssuesSpec
     , getIssuesByLabelsSpec
     ]

upsertIssuesSpec :: AppEnv -> Spec
upsertIssuesSpec env = describe "upsertIssues" $ do
    it "should do nothing when inserting an empty list" $ do
        env & succeeds (upsertIssues [])
        env & issuesAddedRows (upsertIssues []) `equals` 0
    it "should insert issues if its repo exists" $
        env & issuesIncreased (upsertIssues [validIssue]) `equals` True
    it "should leave the issues table unaffected when there's no corresponding repo" $
        env & issuesUnaffected (upsertIssues [invalidIssue]) `equals` True
    it "should update the issue if the same issue already exists" $
        env & issuesRowDifference (upsertIssues [updatedValidIssue]) `equals` [Id 1 `WithId` updatedValidIssue]

getIssuesSpec :: AppEnv -> Spec
getIssuesSpec env = describe "getIssues" $
    it "should return a list of issues with length 1" $
        env & issuesRowCount `equals` 1

getIssuesByLabelsSpec :: AppEnv -> Spec
getIssuesByLabelsSpec env = describe "getIssuesByLabels" $
    it "should return [Id 1 `WithId` updatedValidIssue]" $
        env & getIssuesByLabels [Label "good first issue"] `equals` [Id 1 `WithId` updatedValidIssue]

-- | Returns number of rows currently in the issues database.
issuesRowCount :: App Int
issuesRowCount = length <$> getIssues

-- | Returns the number of rows added to the issue table after an action.
issuesAddedRows :: App a -> App Int
issuesAddedRows = beforeAfter issuesRowCount (-)

-- | Returns True if no rows in the issues table were affected after an action.
issuesUnaffected :: App a -> App Bool
issuesUnaffected = beforeAfter getIssues (==)

-- | Returns True if number of rows in the issues table increased after an action.
issuesIncreased :: App a -> App Bool
issuesIncreased = beforeAfter issuesRowCount (>)

-- | Returns difference between rows of the issue table after and before an action.
issuesRowDifference :: App a -> App [WithId Issue]
issuesRowDifference = beforeAfter getIssues (\\)
