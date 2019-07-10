module Test.Sync
       ( syncSpecs
       ) where

import IW.Core.Repo (RepoOwner (..), RepoName (..))
import IW.Sync.Search (parseIssueUserData)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified GitHub  


syncSpecs :: Spec
syncSpecs = describe "GitHub sync correctness"
    parseIssueUserDataSpec

parseIssueUserDataSpec :: Spec
parseIssueUserDataSpec = describe "parseIssueUserData" $ do
    it "parsing testGitHubIssueUrl return Just (RepoOwner owner123, RepoName repo123)" $ 
        parseIssueUserData testGitHubIssueUrl 
            `shouldBe` Just (RepoOwner "owner123", RepoName "repo123")
    it "parsing testBadGitHubIssueUrl1 should return Nothing" $ 
        parseIssueUserData testBadGitHubIssueUrl1
            `shouldBe` Nothing
    it "parsing testBadGitHubIssueUrl2 return Nothing" $
        parseIssueUserData testBadGitHubIssueUrl2
            `shouldBe` Nothing

testGitHubIssueUrl :: GitHub.URL
testGitHubIssueUrl = GitHub.URL "https://api.github.com/repos/owner123/repo123/issues/1"

testBadGitHubIssueUrl1 :: GitHub.URL
testBadGitHubIssueUrl1 = GitHub.URL "https://api.github.com/randomWord/owner123/repo123/issues/1"

testBadGitHubIssueUrl2 :: GitHub.URL
testBadGitHubIssueUrl2 = GitHub.URL "api.github.com/repos/owner123/repo123/issues/1"
