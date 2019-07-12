module Test.Sync
       ( syncSpecs
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IW.App (AppEnv, notFound)
import IW.Core.Repo (RepoOwner (..), RepoName (..), Category (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (downloadFileImpl)
import IW.Effects.Cabal (getCabalCategoriesImpl, repoCabalUrl)
import IW.Sync.Search (parseIssueUserData)
import Test.Assert (equals, succeeds, failsWith)

import qualified GitHub


syncSpecs :: AppEnv -> Spec
syncSpecs env = describe "GitHub sync correctness" $ do
    parseIssueUserDataSpec
    downloadFileSpec env
    getCabalCategoriesSpec env
    repoCabalUrlSpec

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
       env & downloadFileImpl nonExistentUrl
            `failsWith` notFound
    it "should be equal to issueWantedMain when passed the issueWantedMainContent" $
       env & downloadFileImpl issueWantedMainUrl
            `equals` issueWantedMainContent

getCabalCategoriesSpec :: AppEnv -> Spec
getCabalCategoriesSpec env = describe "getCabalCategories" $ do
    it "should return issueWantedCategories when passed in a valid URL for the issue-wanted cabal file" $
       env & uncurry getCabalCategoriesImpl issueWantedRepo
            `equals` issueWantedCategories
    it "should fail with notFound when passed in a repo that doesn't exist" $
       env & uncurry getCabalCategoriesImpl nonExistentRepo
            `failsWith` notFound
    it "should return SqlArray [] when passed in a repo that has a cabal file without a category field" $
       env & uncurry getCabalCategoriesImpl noCategoryFieldCabalFileRepo
            `equals` []

repoCabalUrlSpec :: Spec
repoCabalUrlSpec = describe "repoCabalUrl" $
    it "should equal issueWantedUrl when passed in issueWantedRepo" $
        uncurry repoCabalUrl issueWantedRepo
            `shouldBe` issueWantedUrl

issueWantedUrl :: Url
issueWantedUrl = Url "https://raw.githubusercontent.com/kowainik/issue-wanted/master/issue-wanted.cabal"

nonExistentUrl :: Url
nonExistentUrl = Url "https://raw.githubusercontent.com/blahblah/noexist123/master/noexist123.kabal"

issueWantedMainUrl :: Url
issueWantedMainUrl = Url "https://raw.githubusercontent.com/kowainik/issue-wanted/master/app/Main.hs"

issueWantedMainContent :: ByteString
issueWantedMainContent = "module Main where\n\nimport qualified IW\n\n\nmain :: IO ()\nmain = IW.main\n"

issueWantedRepo :: (RepoOwner, RepoName)
issueWantedRepo = (RepoOwner "kowainik", RepoName "issue-wanted")

issueWantedCategories :: [Category]
issueWantedCategories = [Category "Web", Category "Application"]

nonExistentRepo :: (RepoOwner, RepoName)
nonExistentRepo = (RepoOwner "blahblah", RepoName "noexist123")

noCategoryFieldCabalFileRepo :: (RepoOwner, RepoName)
noCategoryFieldCabalFileRepo = (RepoOwner "rashadg1030", RepoName "kiiboodoo-api")
