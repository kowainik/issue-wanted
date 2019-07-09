module Test.Sync
       ( syncSpecs
       ) where

import IW.App (AppEnv)
import IW.Core.Repo (RepoOwner (..), RepoName (..))
import IW.Sync.Search (parseIssueUserData)
import Test.Common (joinSpecs)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified GitHub  


testGitHubIssueUrl :: GitHub.URL
testGitHubIssueUrl = GitHub.URL "https://api.github.com/repos/owner123/repo123/issues/1"

syncSpecs :: Spec
syncSpecs = describe "GitHub sync correctness"
    parseIssueUserDataSpec

parseIssueUserDataSpec :: Spec
parseIssueUserDataSpec = describe "parseIssueUserData" $
    it "should return Just (RepoOwner owner123, RepoName repo123)" $ 
        parseIssueUserData testGitHubIssueUrl 
            `shouldBe` Just (RepoOwner "owner123", RepoName "repo123")
