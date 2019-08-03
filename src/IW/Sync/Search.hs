{- | This module provides functions used in fetching Haskell repos and
issues from the GitHub API. Functions with the Search- prefix such as
@fetchAllHaskellRepos@ can be used to make request to the GitHubAPI.
This module also exposes functions that map @github@ library types to our own,
and a parser for extracting the @RepoOwner@ and @RepoName@ from a URL.
-}

module IW.Sync.Search
       ( searchHaskellReposByDate
       , searchAllHaskellIssues
       , searchHaskellIssuesByLabels
       , fromGitHubIssue
       , fromGitHubRepo
       , liftGithubSearchToApp
       , parseUserData
       ) where

import UnliftIO (MonadUnliftIO)
import Data.Time (Day (..))
import GitHub (SearchResult (..), URL (..), RateLimit (..), Limits, Paths, QueryString, executeRequest', limitsRemaining)
import GitHub.Endpoints.RateLimit (rateLimit)

import IW.App (WithError)
import IW.App.Error (githubErrToAppErr, throwError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Time (julianDayToIso)

import qualified GitHub
import qualified Data.Text as T
import qualified Data.Vector as V


liftGithubSearchToApp
    :: forall a env m.
       ( MonadIO m
       , MonadUnliftIO m
       , WithError m
       , WithLog env m
       , Typeable a
       )
    => IO (Either GitHub.Error (SearchResult a))
    -> m [a]
liftGithubSearchToApp githubSearch' = do
    searchLimit <- getSearchRateLimit
    if limitsRemaining searchLimit > 0
        then performSearch
        else delaySearch
  where
    performSearch :: m [a]
    performSearch = liftIO githubSearch' >>= \case
        Left err -> throwError $ githubErrToAppErr err
        Right (SearchResult count vec) -> do
            log Info $ "Fetched total of " <> show count <> " " <> typeName @a <> "s..."
            pure $ V.toList vec

    delaySearch :: m [a]
    delaySearch = do
        log Info "No more requests remaining. Waiting..."
        liftGithubSearchToApp githubSearch'

getSearchRateLimit :: forall m. (MonadIO m, MonadUnliftIO m, WithError m) => m Limits
getSearchRateLimit = liftIO rateLimit >>= \case
    Left err -> throwError $ githubErrToAppErr err
    Right RateLimit{..} -> pure rateLimitSearch

-- | Search all repositories built with the Haskell language by page within a date range.
searchHaskellReposByDate
    :: Day
    -> Day
    -> Int
    -> IO (Either GitHub.Error (SearchResult GitHub.Repo))
searchHaskellReposByDate = githubSearch ["search", "repositories"] "language:haskell"

-- | Search all open issues with Haskell language.
searchAllHaskellIssues
    :: Day
    -> Day
    -> Int
    -> IO (Either GitHub.Error (SearchResult GitHub.Issue))
searchAllHaskellIssues = searchHaskellIssuesByLabels []

-- | Search all open issues with Haskell language and the labels passed in to the function.
searchHaskellIssuesByLabels
    :: [Label]
    -> Day
    -> Day
    -> Int
    -> IO (Either GitHub.Error (SearchResult GitHub.Issue))
searchHaskellIssuesByLabels labels = githubSearch ["search", "issues"] queryString
  where
    queryString :: Text
    queryString = "language:haskell" <> " " <> labelsToSearchQuery labels

    -- | Construct a github search query from a list of labels.
    labelsToSearchQuery :: [Label] -> Text
    labelsToSearchQuery = foldMap (\Label{..} -> "label:\"" <> unLabel <> "\" ")

githubSearch
    :: FromJSON a
    => Paths -- ^ Path
    -> Text  -- ^ Query String
    -> Day   -- ^ From day
    -> Day   -- ^ To day
    -> Int   -- ^ Page
    -> IO (Either GitHub.Error (SearchResult a))
githubSearch paths queryString from to page = executeRequest' $ buildGithubQuery paths queryString from to page

buildGithubQuery
    :: Paths
    -> Text
    -> Day
    -> Day
    -> Int
    -> GitHub.GenRequest 'GitHub.MtJSON 'GitHub.RO a
buildGithubQuery paths queryString from to page = GitHub.query paths queryString'
  where
    queryString' :: QueryString
    queryString' =
        [ ("per_page", Just "100")
        , ("page", Just $ show page)
        , ("q", Just $ encodeUtf8 $ queryString <> " " <> dateRange from to)
        ]

    dateRange :: Day -> Day -> Text
    dateRange from' to' = "created:" <> julianDayToIso from' <> ".." <> julianDayToIso to'

-- | Convert a value of the @GitHub.Repo@ type to a value of our own @Repo@ type.
fromGitHubRepo :: GitHub.Repo -> Repo
fromGitHubRepo githubRepo = Repo
    { repoOwner      = RepoOwner $ GitHub.untagName $ GitHub.simpleOwnerLogin $ GitHub.repoOwner githubRepo
    , repoName       = RepoName $ GitHub.untagName $ GitHub.repoName githubRepo
    , repoDescr      = fromMaybe "" $ GitHub.repoDescription githubRepo
    , repoCategories = SqlArray []
    }

-- | Convert a value of the @GitHub.Issue@ type to a value of our own @Issue@ type.
fromGitHubIssue :: GitHub.Issue -> Maybe Issue
fromGitHubIssue githubIssue = do
    (issueRepoOwner, issueRepoName) <- parseUserData $ GitHub.issueUrl githubIssue
    pure Issue
        { issueNumber = GitHub.unIssueNumber $ GitHub.issueNumber githubIssue
        , issueTitle  = GitHub.issueTitle githubIssue
        , issueBody   = fromMaybe "" $ GitHub.issueBody githubIssue
        , issueLabels = SqlArray
              $ V.toList
              $ Label . GitHub.untagName . GitHub.labelName
              <$> GitHub.issueLabels githubIssue
        , ..
        }

{- | For parsing the @RepoOwner@ and @RepoName@ from a GitHub URL.
Parsing the URL @https://api.github.com/repos/owner/name/issues/1@ should return
@Just (RepoOwner "owner", RepoName "name")@.
-}
parseUserData :: GitHub.URL -> Maybe (RepoOwner, RepoName)
parseUserData (URL url) =
    T.stripPrefix "https://api.github.com/repos/" url
    >>= splitOwnerAndName
  where
    splitOwnerAndName :: Text -> Maybe (RepoOwner, RepoName)
    splitOwnerAndName strippedUrl = do
        owner:name:_ <- Just $ T.splitOn "/" strippedUrl
        guard $ owner /= "" && name /= ""
        pure (RepoOwner owner, RepoName name)
