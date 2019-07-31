{- | This module provides functions used in fetching Haskell repos and
issues from the GitHub API. Functions with the fetch- prefix such as
@fetchAllHaskellRepos@ can be used to make request to the GitHubAPI.
This module also exposes functions that map @github@ library types to our own,
and a parser for extracting the @RepoOwner@ and @RepoName@ from a URL.
-}

module IW.Sync.Search
       ( fetchHaskellReposByDate
       , fetchAllHaskellIssues
       , fetchHaskellIssuesByLabels
       , getSearchRateLimit
       , fromGitHubIssue
       , fromGitHubRepo
       , parseUserData
       ) where

import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)
import Data.Text (strip)
import Data.Time (Day (..))
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import GitHub (SearchResult (..), URL (..), RateLimit (..), Limits, executeRequest', query, limitsRemaining)
import GitHub.Endpoints.RateLimit (rateLimit)

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.App.Error (githubErrToAppErr, throwError)

import qualified GitHub
import qualified Data.Text as T
import qualified Data.Vector as V


githubSearch
    :: forall a env m.
       ( MonadIO m
       , MonadUnliftIO m
       , WithError m
       , WithLog env m
       , FromJSON a
       , Typeable a
       )
    => [Text] -- ^ Paths
    -> Text   -- ^ Query String
    -> Day    -- ^ From day
    -> Day    -- ^ To day
    -> Int    -- ^ Page
    -> m [a]
githubSearch paths queryString from to page = do
    searchLimit <- getSearchRateLimit
    if limitsRemaining searchLimit > 0 then
        do
            log Info $ "Search rate limit information: " <> show searchLimit
            liftIO (executeRequest' $ query paths queries) >>= \case
                Left err -> throwError $ githubErrToAppErr err
                Right (SearchResult count vec) -> do
                    log Info $ "Query executed with the following paths and query parameters: " <> show paths <> " " <> show queries
                    log Info $ "Fetched total of " <> show count <> " " <> typeName @a <> "s..."
                    pure $ V.toList vec
    else
        do
            log Info $ "No more requests remaining. Waiting for one minute..."
            threadDelay 60000000
            githubSearch paths queryString from to page


  where
    queries :: [(ByteString, Maybe ByteString)]
    queries = pagination <> [("q", Just $ encodeUtf8 $ strip queryString <> " " <> dateRange from to)]

    pagination :: [(ByteString, Maybe ByteString)]
    pagination =
        [ ("per_page", Just "100")
        , ("page", Just $ show page)
        ]

    dateRange :: Day -> Day -> Text
    dateRange from' to' = "created:" <> julianDayToIso from' <> ".." <> julianDayToIso to'

    julianDayToIso :: Day -> Text
    julianDayToIso = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

getSearchRateLimit
    :: forall m.
       ( MonadIO m
       , MonadUnliftIO m
       , WithError m
       )
    => m Limits
getSearchRateLimit = liftIO rateLimit >>= \case
    Left err -> throwError $ githubErrToAppErr err
    Right RateLimit{..} -> pure rateLimitSearch

-- | Fetch all repositories built with the Haskell language by page within a date range.
fetchHaskellReposByDate
    :: forall env m.
       ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => Day
    -> Day
    -> Int
    -> m [GitHub.Repo]
fetchHaskellReposByDate from to page = githubSearch paths queryString from to page
  where
    paths :: [Text]
    paths = ["search", "repositories"]

    queryString :: Text
    queryString = "language:haskell"

-- | Fetch all open issues with Haskell language and the labels passed in to the function.
fetchHaskellIssuesByLabels
    :: forall env m.
       ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => [Label]
    -> Day
    -> Day
    -> Int
    -> m [GitHub.Issue]
fetchHaskellIssuesByLabels labels from to page = githubSearch paths queryString from to page
  where
    paths :: [Text]
    paths = ["search", "issues"]

    queryString :: Text
    queryString = "language:haskell" <> " " <> labelsToSearchQuery labels

    -- | Construct a github search query from a list of labels.
    labelsToSearchQuery :: [Label] -> Text
    labelsToSearchQuery = foldMap (\Label{..} -> "label:\"" <> unLabel <> "\" ")

-- | Fetch all open issues with Haskell language.
fetchAllHaskellIssues
    :: ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => Day
    -> Day
    -> Int
    -> m [GitHub.Issue]
fetchAllHaskellIssues = fetchHaskellIssuesByLabels []

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
