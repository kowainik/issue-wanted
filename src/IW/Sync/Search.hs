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
       , fromGitHubIssue
       , fromGitHubRepo
       , parseUserData
       ) where

import Data.Time (Day (..))
import Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import GitHub (SearchResult (..), URL (..))
import GitHub.Data.RateLimit (RateLimit (..), Limits)
import GitHub.Data.Request (query)
import GitHub.Request (executeRequest')
import GitHub.Endpoints.Search (searchIssues)
import GitHub.Endpoints.RateLimit (rateLimit)

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.App.Error (githubErrToAppErr, throwError)

import qualified GitHub
import qualified Data.Text as T
import qualified Data.Vector as V


-- | Fetch all repositories built with the Haskell language by page within a date range.
fetchHaskellReposByDate
    :: ( MonadIO m
       , WithError m
       , WithLog env m
       )
    => Day
    -> Day
    -> Int
    -> m [GitHub.Repo]
fetchHaskellReposByDate from to page = liftGitHubSearchToApp $ searchHaskellReposByDate from to page

-- | Fetch all open issues with Haskell language and the labels passed in to the function.
fetchHaskellIssuesByLabels
    :: ( MonadIO m
       , WithError m
       , WithLog env m
       )
    => [Label]
    -> m [GitHub.Issue]
fetchHaskellIssuesByLabels = liftGitHubSearchToApp . searchHaskellIssuesByLabels

-- | Fetch all open issues with Haskell language.
fetchAllHaskellIssues
    :: ( MonadIO m
       , WithError m
       , WithLog env m
       )
    => m [GitHub.Issue]
fetchAllHaskellIssues = fetchHaskellIssuesByLabels []

-- | Lift a github search action to work within our monad stack.
liftGitHubSearchToApp
    :: forall a env m.
       ( MonadIO m
       , WithError m
       , WithLog env m
       , Typeable a
       )
    => IO (Either GitHub.Error (SearchResult a))
    -> m [a]
liftGitHubSearchToApp searchAction = liftIO searchAction >>= \case
    Left err -> throwError $ githubErrToAppErr err
    Right (SearchResult count vec) -> do
        log Info $ "Fetching total of " <> show count <> " " <> typeName @a <> "s..."
        searchLimit <- getSearchRateLimit
        log Info $ "Search rate limit information: " <> show searchLimit
        pure $ V.toList vec

getSearchRateLimit
    :: forall m.
       ( MonadIO m
       , WithError m
       )
    => m Limits
getSearchRateLimit = liftIO rateLimit >>= \case
    Left err -> throwError $ githubErrToAppErr err
    Right RateLimit{..} -> pure rateLimitSearch

githubSearch :: FromJSON a => [Text] -> [(ByteString, Maybe ByteString)] -> IO (Either GitHub.Error (SearchResult a))
githubSearch paths queries = executeRequest' $ query paths queries

-- | Search all repositories built with the Haskell language by page number within a date range.
searchHaskellReposByDate :: Day -> Day -> Int -> IO (Either GitHub.Error (SearchResult GitHub.Repo))
searchHaskellReposByDate from to page = githubSearch paths queries
  where
    paths :: [Text]
    paths = ["search", "repositories"]

    queries :: [(ByteString, Maybe ByteString)]
    queries =
        [ ("q", Just searchString)
        , ("per_page", Just "100")
        , ("page", Just $ show page)
        ]

    searchString :: ByteString
    searchString = "language:haskell created:" <> dateRange

    dateRange :: ByteString
    dateRange = julianDayToIsoBS from <> ".." <> julianDayToIsoBS to

    julianDayToIsoBS :: Day -> ByteString
    julianDayToIsoBS = fromString . formatTime defaultTimeLocale (iso8601DateFormat Nothing)

-- | Convert a value of the @GitHub.Repo@ type to a value of our own @Repo@ type.
fromGitHubRepo :: GitHub.Repo -> Repo
fromGitHubRepo githubRepo = Repo
    { repoOwner      = RepoOwner $ GitHub.untagName $ GitHub.simpleOwnerLogin $ GitHub.repoOwner githubRepo
    , repoName       = RepoName $ GitHub.untagName $ GitHub.repoName githubRepo
    , repoDescr      = fromMaybe "" $ GitHub.repoDescription githubRepo
    , repoCategories = SqlArray []
    }

-- | Search for all open Haskell issues with the corresponding labels.
searchHaskellIssuesByLabels :: [Label] -> IO (Either GitHub.Error (SearchResult GitHub.Issue))
searchHaskellIssuesByLabels labels = searchIssues $ "language:haskell is:open " <> labelsToSearchQuery labels

-- | Construct a github search query from a list of labels.
labelsToSearchQuery :: [Label] -> Text
labelsToSearchQuery = foldMap (\Label{..} -> "label:\"" <> unLabel <> "\" ")

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
