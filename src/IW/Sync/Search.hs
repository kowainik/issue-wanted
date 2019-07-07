module IW.Sync.Search
       ( fetchAllHaskellRepos
       , fetchAllHaskellIssues
       , fetchHaskellIssuesByLabels
       , fromgitHubIssue
       , fromGitHubRepo
       , parseIssueUserData
       ) where

import GitHub (SearchResult (..), URL (..))        
import GitHub.Endpoints.Search (searchRepos, searchIssues)

import IW.App (WithError)
import IW.Core.Issue (Issue (..), Label (..))
import IW.Core.Repo (Repo (..), RepoOwner (..), RepoName (..))
import IW.Core.SqlArray (SqlArray (..))
import IW.Db (WithDb)
import IW.App.Error (githubErrToAppErr, throwError)

import qualified GitHub
import qualified Data.Text as T
import qualified Data.Vector as V


-- | Fetch all repositories built with the Haskell language.
fetchAllHaskellRepos 
    :: ( WithDb env m
       , WithError m
       , WithLog env m 
       ) 
    => m [GitHub.Repo]
fetchAllHaskellRepos = liftGitHubSearchToApp searchHaskellRepos 

-- | Fetch all open issues with Haskell language and the labels passed in to the function.
fetchHaskellIssuesByLabels 
    :: ( WithDb env m
       , WithError m
       , WithLog env m
       ) 
    => [Label] 
    -> m [GitHub.Issue]
fetchHaskellIssuesByLabels = liftGitHubSearchToApp . searchHaskellIssuesByLabels

-- | Fetch all open issues with Haskell language.
fetchAllHaskellIssues 
    :: ( WithDb env m
       , WithError m
       , WithLog env m
       ) 
    => m [GitHub.Issue]
fetchAllHaskellIssues = fetchHaskellIssuesByLabels []

-- | Lift a github search action to work within our monad stack.
liftGitHubSearchToApp 
    :: forall a env m.
       ( WithDb env m
       , WithError m
       , WithLog env m
       , Typeable a
       ) 
    => IO (Either GitHub.Error (SearchResult a)) 
    -> m [a]
liftGitHubSearchToApp githubSearch = liftIO githubSearch >>= \case
    Left err -> throwError $ githubErrToAppErr err 
    Right (SearchResult count vec) -> do
        log Info $ "Fetching total of " <> show count <> " " <> typeName @a <> "s..." 
        pure $ V.toList vec

-- | Search all repositories built with the Haskell language.
searchHaskellRepos :: IO (Either GitHub.Error (SearchResult GitHub.Repo))
searchHaskellRepos = searchRepos "language:haskell"

-- | Construct a github search query from a list of labels.
labelsToSearchQuery :: [Label] -> Text
labelsToSearchQuery = foldMap (\Label{..} -> "label:\"" <> unLabel <> "\" ")

-- | Search for all open Haskell issues with the corresponding labels.
searchHaskellIssuesByLabels :: [Label] -> IO (Either GitHub.Error (SearchResult GitHub.Issue))
searchHaskellIssuesByLabels labels = searchIssues $ "language:haskell is:open " <> labelsToSearchQuery labels

-- | Convert a value of the @GitHub.Repo@ type to a value of our own @Repo@ type.
fromGitHubRepo :: GitHub.Repo -> Repo
fromGitHubRepo githubRepo = Repo
    { repoOwner      = RepoOwner $ GitHub.untagName $ GitHub.simpleOwnerLogin $ GitHub.repoOwner githubRepo
    , repoName       = RepoName  $ GitHub.untagName $ GitHub.repoName githubRepo
    , repoDescr      = fromMaybe "" $ GitHub.repoDescription githubRepo
    , repoCategories = SqlArray []
    }

-- | Convert a value of the @GitHub.Issue@ type to a value of our own @Issue@ type.
fromgitHubIssue :: GitHub.Issue -> Maybe Issue
fromgitHubIssue githubIssue = do
    (issueRepoOwner, issueRepoName) <- parseIssueUserData $ GitHub.issueUrl githubIssue
    pure Issue 
        { issueNumber = GitHub.unIssueNumber $ GitHub.issueNumber githubIssue
        , issueTitle  = GitHub.issueTitle githubIssue 
        , issueBody   = fromMaybe "" $ GitHub.issueBody githubIssue
        , issueLabels = SqlArray 
              $ V.toList 
              $ (\GitHub.IssueLabel{..} -> Label $ GitHub.untagName labelName) 
              <$> GitHub.issueLabels githubIssue
        , ..
        }

-- | For parsing the @issueRepoOwner@ and @issueRepoName@ from the issue's URL.
parseIssueUserData :: GitHub.URL -> Maybe (RepoOwner, RepoName)
parseIssueUserData (URL url) =
    T.stripPrefix "https://api.github.com/repos/" url 
    >>= stripGitSuffix 
    >>= splitOwnerAndName
  where
    splitOwnerAndName :: Text -> Maybe (RepoOwner, RepoName)
    splitOwnerAndName strippedUrl =
        let (owner, name) = fst . T.breakOn "/" . T.drop 1 <$> T.breakOn "/" strippedUrl 
        in guard (owner /= "" && name /= "") *> Just (RepoOwner owner, RepoName name)

    stripGitSuffix :: Text -> Maybe Text
    stripGitSuffix x = whenNothing (T.stripSuffix ".git" x) (Just x)
