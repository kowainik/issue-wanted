module Test.Sync
       ( syncSpecs
       ) where

import IW.Core.Repo (RepoOwner (..), RepoName (..))
import IW.Sync.Search (parseIssueUserData)
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified GitHub  


testGitHubIssueUrl :: GitHub.URL
testGitHubIssueUrl = GitHub.URL "https://api.github.com/repos/owner123/repo123/issues/1"

testBadGitHubIssueUrl1 :: GitHub.URL
testBadGitHubIssueUrl1 = GitHub.URL "https://api.github.com/repos/owner123/issues/1"

testBadGitHubIssueUrl2 :: GitHub.URL
testBadGitHubIssueUrl2 = GitHub.URL "https://api.github.com/randomWord/owner123/repo123/issues/1"

testBadGitHubIssueUrl3 :: GitHub.URL
testBadGitHubIssueUrl3 = GitHub.URL "api.github.com/repos/owner123/repo123/issues/1"

syncSpecs :: Spec
syncSpecs = describe "GitHub sync correctness"
    parseIssueUserDataSpec

parseIssueUserDataSpec :: Spec
parseIssueUserDataSpec = describe "parseIssueUserData" $ do
    it "should return Just (RepoOwner owner123, RepoName repo123)" $ 
        parseIssueUserData testGitHubIssueUrl 
            `shouldBe` Just (RepoOwner "owner123", RepoName "repo123")
    it "should return Nothing" $ 
        parseIssueUserData testBadGitHubIssueUrl1
            `shouldBe` Nothing
    it "should return Nothing" $
        parseIssueUserData testBadGitHubIssueUrl2
            `shouldBe` Nothing
    it "should return Nothing" $
        parseIssueUserData testBadGitHubIssueUrl3
            `shouldBe` Nothing
