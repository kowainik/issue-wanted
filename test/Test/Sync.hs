module Test.Sync
       ( syncSpecs
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IW.App (AppEnv, notFound)
import IW.Core.Repo (RepoOwner (..), RepoName (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (downloadFileImpl)
import IW.Sync.Search (parseIssueUserData)
import Test.Assert (succeeds, failsWith)

import qualified GitHub  


syncSpecs :: AppEnv -> Spec
syncSpecs env = describe "GitHub sync correctness" $ do
    parseIssueUserDataSpec
    downloadFileSpec env 

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

downloadFileSpec :: AppEnv -> Spec
downloadFileSpec env = describe "downloadFile" $ do
    it "should succeed with 200 status code when passed a valid Url" $
       env & succeeds (downloadFileImpl issueWantedUrl)
    it "should fail with notFound error when passed a non-existent Url" $
       env & downloadFileImpl nonExistentUrl `failsWith` notFound 

issueWantedUrl :: Url
issueWantedUrl = Url "https://raw.githubusercontent.com/kowainik/issue-wanted/master/issue-wanted.cabal"

nonExistentUrl :: Url
nonExistentUrl = Url "https://raw.githubusercontent.com/blahblah/noexist123/master/noexist123.kabal"
