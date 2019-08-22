module Test.Sync
       ( syncSpecs
       ) where

import Test.Hspec (Spec, describe, it, shouldBe)

import IW.App (AppEnv, AppErrorType (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..), Category (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (downloadFileImpl)
import IW.Effects.Cabal (getCabalCategoriesImpl, repoCabalUrl)
import IW.Sync.Search (parseUserData, mkRepoCabalUrl)
import Test.Assert (equals, succeeds, failsWith)

import qualified GitHub


syncSpecs :: AppEnv -> Spec
syncSpecs env = describe "GitHub sync correctness" $ do
    parseIssueUserDataSpec env
    downloadFileSpec env
    getCabalCategoriesSpec env
    repoCabalUrlSpec

parseIssueUserDataSpec :: AppEnv -> Spec
parseIssueUserDataSpec env = describe "parseIssueUserData" $ do
    it "parsing testGitHubUrl1 should return Just (RepoOwner owner123, RepoName repo123)" $
        env & parseUserData testGitHubUrl1
            `equals` (RepoOwner "owner123", RepoName "repo123")
    it "parsing testGitHubUrl2 should return Just (RepoOwner owner123, RepoName repo123)" $
        env & parseUserData testGitHubUrl2
            `equals` (RepoOwner "owner123", RepoName "repo123")
    it "parsing testBadGitHubUrl1 should fail with NotFound error" $
        env & parseUserData testBadGitHubUrl1
            `failsWith` NotFound
    it "parsing testBadGitHubUrl2 should fail with NotFound error" $
        env & parseUserData testBadGitHubUrl2
            `failsWith` NotFound

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
            `failsWith` UrlDownloadFailed nonExistentCabalUrl
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
       env & getCabalCategoriesImpl issueWantedRepo
            `equals` issueWantedCategories
    it "should return [] when passed in a repo that doesn't exist" $
       env & getCabalCategoriesImpl nonExistentRepo
            `failsWith` (UrlDownloadFailed $ repoCabalUrl nonExistentRepo)
    it "should return [] when passed in a repo that has a cabal file without a category field" $
       env & getCabalCategoriesImpl noCategoryFieldCabalFileRepo
            `equals` []

repoCabalUrlSpec :: Spec
repoCabalUrlSpec = describe "repoCabalUrl" $ do
    it "should equal issueWantedCabalUrl when passed in issueWantedRepo" $
        repoCabalUrl issueWantedRepo
            `shouldBe` issueWantedCabalUrl
    it "should equal nonExistentCabalUrl when passed in nonExistentRepo" $
        repoCabalUrl nonExistentRepo
            `shouldBe` nonExistentCabalUrl

-- | @Repo@ data for the @issue-wanted@ project.
issueWantedRepo :: Repo
issueWantedRepo = Repo
    { repoOwner      = RepoOwner "kowainik"
    , repoName       = RepoName "issue-wanted"
    , repoDescr      = ""
    , repoCategories = SqlArray [Category "Web", Category "Application"]
    , repoCabalUrl   = mkRepoCabalUrl (RepoOwner "kowainik") (RepoName "issue-wanted") Nothing
    }

-- | @Repo@ data for a repository doesn't exist.
nonExistentRepo :: Repo
nonExistentRepo = Repo
    { repoOwner      = RepoOwner "blahblah"
    , repoName       = RepoName "noexist123"
    , repoDescr      = ""
    , repoCategories = SqlArray []
    , repoCabalUrl   = mkRepoCabalUrl (RepoOwner "blahblah") (RepoName "noexist123") Nothing
    }

-- | @Repo@ data for a repository that does exist,
-- but doesn't have a @.cabal@ file with a @category@ field.
noCategoryFieldCabalFileRepo :: Repo
noCategoryFieldCabalFileRepo = Repo
    { repoOwner      = RepoOwner "rashadg1030"
    , repoName       = RepoName "kiiboodoo-api"
    , repoDescr      = ""
    , repoCategories = SqlArray []
    , repoCabalUrl   = mkRepoCabalUrl (RepoOwner "rashadg1030") (RepoName "kiiboodoo-api") Nothing
    }

-- | Represents the categories in @catgory@ field of the @issue-wanted@ @.cabal@ file.
issueWantedCategories :: [Category]
issueWantedCategories = [Category "Web", Category "Application"]
