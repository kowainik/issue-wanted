{-# LANGUAGE MultiWayIf #-}

{- | This module provides functions used in fetching Haskell repos and
issues from the GitHub API. Functions with the Search- prefix such as
@fetchAllHaskellRepos@ can be used to make request to the GitHubAPI.
This module also exposes functions that map @github@ library types to our own,
and a parser for extracting the @RepoOwner@ and @RepoName@ from a URL.
-}

module IW.Sync.Search
       ( searchAllHaskellRepos
       , searchAllHaskellIssues
       , searchAllHaskellIssuesByLabels
       , parseUserData
       ) where

import Data.Time (Day (..), addDays)
import GitHub (SearchResult (..), URL (..), RateLimit (..), Limits, Paths, QueryString,
               executeRequest', limitsRemaining)
import GitHub.Endpoints.RateLimit (rateLimit)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Concurrent (threadDelay)

import IW.App (WithError, AppErrorType (..), githubErrToAppErr, throwError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Time (julianDayToIso)

import qualified GitHub
import qualified Data.Text as T
import qualified Data.Vector as V

{- HLINT ignore "Use prec" -}


-- | Search all Haskell repositories starting from the most recent day.
searchAllHaskellRepos
    :: ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => Day     -- ^ The function starts from this day and goes back in time
    -> Integer -- ^ The size of the date interval that is used in the search
    -> m [Repo]
searchAllHaskellRepos recent interval = do
    githubRepos <- githubSearchAll ["search", "repositories"] "language:haskell" recent interval []
    pure $ fromGitHubRepo <$> githubRepos

-- | Search all Haskell open issues starting from the most recent day.
searchAllHaskellIssues
    :: ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => Day
    -> Integer
    -> m [Issue]
searchAllHaskellIssues = searchAllHaskellIssuesByLabels []

-- | Search all open Haskell issues with the corresponding labels.
searchAllHaskellIssuesByLabels
    :: ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       )
    => [Label] -- ^ The function will search for issues with these labels
    -> Day
    -> Integer
    -> m [Issue]
searchAllHaskellIssuesByLabels labels recent interval = do
     githubIssues <- githubSearchAll ["search", "issues"] queryString recent interval []
     mapM fromGitHubIssue githubIssues
  where
    queryString :: Text
    queryString = "language:haskell state:open" <> labelsToSearchQuery labels

    -- | Construct a github search query from a list of labels.
    labelsToSearchQuery :: [Label] -> Text
    labelsToSearchQuery = foldMap (\Label{..} -> "label:\"" <> unLabel <> "\" ")

githubSearchAll
    :: forall a env m.
       ( MonadUnliftIO m
       , WithError m
       , WithLog env m
       , FromJSON a
       , Typeable a
       )
    => Paths   -- ^ Query paths
    -> Text    -- ^ Query properties
    -> Day     -- ^ The function starts at this day and goes back in time by the size of the interval
    -> Integer -- ^ The date interval
    -> [a]     -- ^ A list of accumulated results used in recursive calls to this function
    -> m [a]
githubSearchAll paths properties recent interval acc = do
    -- | Search for first page of the query and check the result count to see what to do next.
    SearchResult count vec <- executeGithubSearch paths properties firstDay recent 1
    let firstPage = V.toList vec
        -- | If count is 0, then all results for the given properties have been obtained.
    if | count == 0   -> do
            log I "All results obtained"
            pure acc
        -- | If count is less than or equal to 1000, then the interval is good and we can get
        -- the rest of the results on pages 2 to 10.
       | count <= 1000 -> do
            listOfSearchResult <- mapM (executeGithubSearch paths properties firstDay recent) [2..10]
            let remainingPages = concatMap searchResultToList listOfSearchResult
            -- | Recursive call with a new @recent@ arguement and a new @acc@ arguement
            -- representing all pages accumulated up to this point.
            githubSearchAll paths properties (pred firstDay) interval (firstPage <> remainingPages <> acc)
        -- | Otherwise, call the function with the same arguments but a smaller interval.
       | otherwise     -> githubSearchAll paths properties recent (pred interval) acc
  where
    -- | The first day of the search interval. It's calculated by subtracting the size of the
    -- interval from the most recent day of the search interval.
    firstDay :: Day
    firstDay = negate interval `addDays` recent

    searchResultToList :: SearchResult a -> [a]
    searchResultToList (SearchResult _ vec) = V.toList vec

-- | Executes a GitHub search action within the context of the @App@ monad.
executeGithubSearch
    :: ( MonadIO m
       , MonadUnliftIO m
       , WithError m
       , WithLog env m
       , FromJSON a
       )
    => Paths -- ^ Query paths
    -> Text  -- ^ Query properties
    -> Day   -- ^ The first day of the date interval
    -> Day   -- ^ The last day of the date interval
    -> Int   -- ^ The number of the page to be searched
    -> m (SearchResult a)
executeGithubSearch paths properties from to page = do
    searchLimit <- getSearchRateLimit
    log I $ "Current search limit information: " <> show searchLimit
    if limitsRemaining searchLimit > 0
        then do
            log D $ "Searching GitHub API with following request: " <> showGithubQuery paths properties from to page
            liftIO (githubSearch paths properties from to page) >>= \case
                Left err -> throwError $ githubErrToAppErr err
                Right searchRes -> pure searchRes
        else do
            log I "API limit reached. Delaying search..."
            threadDelay 60000000
            executeGithubSearch paths properties from to page

-- | Performs a query against the GitHub Search API.
githubSearch
    :: FromJSON a
    => Paths
    -> Text
    -> Day
    -> Day
    -> Int
    -> IO (Either GitHub.Error (SearchResult a))
githubSearch paths properties from to page = executeRequest' $ buildGithubQuery paths properties from to page

-- | Useful function for constructing a GitHub query and showing it as text.
showGithubQuery
    :: Paths
    -> Text
    -> Day
    -> Day
    -> Int
    -> Text
showGithubQuery paths properties from to page = show $ buildGithubQuery paths properties from to page

-- | Function for building a GitHub search query.
buildGithubQuery
    :: Paths -- ^ Query paths
    -> Text  -- ^ Query properties
    -> Day   -- ^ The first day of the date interval
    -> Day   -- ^ The last day of the date interval
    -> Int   -- ^ The number of the page to be searched
    -> GitHub.GenRequest 'GitHub.MtJSON 'GitHub.RO a
buildGithubQuery paths properties from to page = GitHub.query paths queryString
  where
    queryString :: QueryString
    queryString =
        [ ("per_page", Just "100")
        , ("page", Just $ show page)
        , ("q", Just $ encodeUtf8 $ properties <> " " <> dateRange from to)
        ]

    dateRange :: Day -> Day -> Text
    dateRange from' to' = "created:" <> julianDayToIso from' <> ".." <> julianDayToIso to'

-- | Function for fetching the current rate limit information for the GitHub Search API.
getSearchRateLimit :: (MonadIO m, MonadUnliftIO m, WithError m) => m Limits
getSearchRateLimit = liftIO rateLimit >>= \case
    Left err -> throwError $ githubErrToAppErr err
    Right RateLimit{..} -> pure rateLimitSearch

-- | Convert a value of the @GitHub.Repo@ type to a value of our own @Repo@ type.
fromGitHubRepo :: GitHub.Repo -> Repo
fromGitHubRepo githubRepo = Repo
    { repoOwner      = RepoOwner $ GitHub.untagName $ GitHub.simpleOwnerLogin $ GitHub.repoOwner githubRepo
    , repoName       = RepoName $ GitHub.untagName $ GitHub.repoName githubRepo
    , repoDescr      = fromMaybe "" $ GitHub.repoDescription githubRepo
    , repoCategories = SqlArray []
    }

-- | Convert a value of the @GitHub.Issue@ type to a value of our own @Issue@ type.
fromGitHubIssue :: (WithError m, WithLog env m) => GitHub.Issue -> m Issue
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
parseUserData :: (WithError m, WithLog env m) => GitHub.URL -> m (RepoOwner, RepoName)
parseUserData (URL url) = case maybeUserData of
    Nothing -> do
        log E "Couldn't parse user data from URL"
        throwError NotFound
    Just userData -> pure userData
  where
    maybeUserData :: Maybe (RepoOwner, RepoName)
    maybeUserData = T.stripPrefix "https://api.github.com/repos/" url >>=
        splitOwnerAndName

    splitOwnerAndName :: Text -> Maybe (RepoOwner, RepoName)
    splitOwnerAndName strippedUrl = do
        owner:name:_ <- Just $ T.splitOn "/" strippedUrl
        guard $ owner /= "" && name /= ""
        pure (RepoOwner owner, RepoName name)
