module IW.Sync.Search where

import GitHub (Error, Repo, SearchResult, Issue)
import GitHub.Endpoints.Search (searchRepos, searchIssues)

-- | Fetch all repositories with Haskell language
fetchAllHaskellRepos :: IO (Either Error (SearchResult Repo))
fetchAllHaskellRepos = searchRepos "language:haskell"

-- | Fetch all repositories with Haskell language and label "help wanted"
fetchHaskellReposHW :: IO (Either Error (SearchResult Repo))
fetchHaskellReposHW = searchRepos "language:haskell help-wanted-issues:>0"

-- | Fetch all repositories with Haskell language and label "good first issue"
fetchHaskellReposGFI :: IO (Either Error (SearchResult Repo))
fetchHaskellReposGFI = searchRepos "language:haskell good-first-issues:>0"

-- | Fetch all open issues with Haskell language and the labels passed in to the function
fetchOpenIssuesWithLabels :: [Text] -> IO (Either Error (SearchResult Issue))
fetchOpenIssuesWithLabels labels = searchIssues $ "language:haskell is:open " <> labelQualifiers
  where
    labelQualifiers :: Text
    labelQualifiers = foldMap (\x -> "label:\"" <> x <> "\" ") labels   

-- | Fetch all open issues with Haskell language and label "help wanted"
fetchHelpWanted :: IO (Either Error (SearchResult Issue))
fetchHelpWanted = fetchOpenIssuesWithLabels ["help wanted"]

-- | Fetch all open issues with Haskell language and label "good first issue"
fetchGoodFirstIssue :: IO (Either Error (SearchResult Issue))
fetchGoodFirstIssue = fetchOpenIssuesWithLabels ["good first issue"]
