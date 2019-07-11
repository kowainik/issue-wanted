module Test.Sync
       ( syncSpecs
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IW.App (AppEnv, urlDownloadFailedError)
import IW.Core.Repo (RepoOwner (..), RepoName (..), Category (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (downloadFileImpl)
import IW.Effects.Cabal (getCabalCategoriesImpl, repoCabalUrl)
import IW.Sync.Search (parseUserData)
import Test.Assert (equals, succeeds)

import qualified GitHub


syncSpecs :: AppEnv -> Spec
syncSpecs env = describe "GitHub sync correctness" $ do
    parseIssueUserDataSpec
    downloadFileSpec env
    getCabalCategoriesSpec env
    repoCabalUrlSpec

parseIssueUserDataSpec :: Spec
parseIssueUserDataSpec = describe "parseIssueUserData" $ do
    it "parsing testGitHubUrl1 should return Just (RepoOwner owner123, RepoName repo123)" $
        parseUserData testGitHubUrl1
            `shouldBe` Just (RepoOwner "owner123", RepoName "repo123")
    it "parsing testGitHubUrl2 should return Just (RepoOwner owner123, RepoName repo123)" $
        parseUserData testGitHubUrl2
            `shouldBe` Just (RepoOwner "owner123", RepoName "repo123")
    it "parsing testBadGitHubUrl1 return Nothing" $
        parseUserData testBadGitHubUrl1
            `shouldBe` Nothing
    it "parsing testBadGitHubUrl2 return Nothing" $
        parseUserData testBadGitHubUrl2
            `shouldBe` Nothing

-- | A @GitHub.Url@ for a repo with a valid format.
testGitHubUrl1 :: GitHub.URL
testGitHubUrl1 = GitHub.URL "https://api.github.com/repos/owner123/repo123"

-- | A @GitHub.Url@ for an issue with a valid format.
testGitHubUrl2 :: GitHub.URL
testGitHubUrl2 = GitHub.URL "https://api.github.com/repos/owner123/repo123/issues/1"

-- | An invalid @GitHub.Url@ that uses @http@ instead of @https@.
testBadGitHubUrl1 :: GitHub.URL
testBadGitHubUrl1 = GitHub.URL "http://api.github.com/repos/owner123/repo123/issues/1"

-- | An invalid @GitHub.Url@ with an extra directory in the path.
testBadGitHubUrl2 :: GitHub.URL
testBadGitHubUrl2 = GitHub.URL "http://api.github.com/randomWord/repos/owner123/repo123"

downloadFileSpec :: AppEnv -> Spec
downloadFileSpec env = describe "downloadFile" $ do
    it "should succeed with 200 status code when passed a valid Url" $
       env & succeeds (downloadFileImpl issueWantedCabalUrl)
    it "should fail with UrlDownloadFailed error when passed a non-existent Url" $
       env & downloadFileImpl nonExistentCabalUrl
            `failsWith` urlDownloadFailedError nonExistentCabalUrl
    it "should be equal to issueWantedMainContent when passed the issueWantedMainUrl" $
       env & downloadFileImpl issueWantedMainUrl
            `equals` issueWantedMainContent

-- | A @Url@ for the @issue-wanted@ @.cabal@ file.
issueWantedCabalUrl :: Url
issueWantedCabalUrl = Url "https://raw.githubusercontent.com/kowainik/issue-wanted/master/issue-wanted.cabal"

-- | A @Url@ for a non-existent @.cabal@ file.
nonExistentCabalUrl :: Url
nonExistentCabalUrl = Url "https://raw.githubusercontent.com/blahblah/noexist123/master/noexist123.cabal"

-- | A @Url@ for the @issue-wanted@ @Main.hs@ file.
issueWantedMainUrl :: Url
issueWantedMainUrl = Url "https://raw.githubusercontent.com/kowainik/issue-wanted/master/app/Main.hs"

-- | The contents of the @issue-wanted@ @Main.hs@ file.
issueWantedMainContent :: ByteString
issueWantedMainContent = "module Main where\n\nimport qualified IW\n\n\nmain :: IO ()\nmain = IW.main\n"

getCabalCategoriesSpec :: AppEnv -> Spec
getCabalCategoriesSpec env = describe "getCabalCategories" $ do
    it "should return issueWantedCategories when passed in a valid URL for the issue-wanted cabal file" $
       env & uncurry getCabalCategoriesImpl issueWantedRepo
            `equals` issueWantedCategories
    it "should return [] when passed in a repo that doesn't exist" $
       env & uncurry getCabalCategoriesImpl nonExistentRepo
            `equals` []
    it "should return [] when passed in a repo that has a cabal file without a category field" $
       env & uncurry getCabalCategoriesImpl noCategoryFieldCabalFileRepo
            `equals` []

repoCabalUrlSpec :: Spec
repoCabalUrlSpec = describe "repoCabalUrl" $ do
    it "should equal issueWantedCabalUrl when passed in issueWantedRepo" $
        uncurry repoCabalUrl issueWantedRepo
            `shouldBe` issueWantedCabalUrl
    it "should equal nonExistentCabalUrl when passed in nonExistentRepo" $
        uncurry repoCabalUrl nonExistentRepo
            `shouldBe` nonExistentCabalUrl

-- | A tuple of @Repo@ data for the @issue-wanted@ project.
issueWantedRepo :: (RepoOwner, RepoName)
issueWantedRepo = (RepoOwner "kowainik", RepoName "issue-wanted")

-- | A tuple of @Repo@ data for a repository doesn't exist.
nonExistentRepo :: (RepoOwner, RepoName)
nonExistentRepo = (RepoOwner "blahblah", RepoName "noexist123")

-- | A tuple of @Repo@ data for a repository that does exist,
-- but doesn't have a @.cabal@ file with a @category@ field.
noCategoryFieldCabalFileRepo :: (RepoOwner, RepoName)
noCategoryFieldCabalFileRepo = (RepoOwner "rashadg1030", RepoName "kiiboodoo-api")

-- | Represents the categories in @catgory@ field of the @issue-wanted@ @.cabal@ file.
issueWantedCategories :: [Category]
issueWantedCategories = [Category "Web", Category "Application"]
