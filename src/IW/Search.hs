{-# LANGUAGE OverloadedStrings #-}

module IW.Search where

import GitHub (Error, Repo, SearchResult, Issue)
import GitHub.Endpoints.Search (searchRepos, searchIssues)


-- | Fetch all repositories with Haskell language
fetchAllHaskellRepos :: IO (Either Error (SearchResult Repo))
fetchAllHaskellRepos = searchRepos "language:haskell"

-- | Fetch all repositories with Haskell language and label "help-wanted"
fetchHaskellReposHW :: IO (Either Error (SearchResult Repo))
fetchHaskellReposHW = searchRepos "language:haskell help-wanted-issues:>0"

-- | Fetch all repositories with Haskell language and label "good-first-issue"
fetchHaskellReposGFI :: IO (Either Error (SearchResult Repo))
fetchHaskellReposGFI = searchRepos "language:haskell good-first-issues:>0"

-- | Fetch all issues with Haskell language and label "help-wanted"
fetchHelpWanted :: IO (Either Error (SearchResult Issue))
fetchHelpWanted = searchIssues "language:haskell label:\"help wanted\""

-- | Fetch all issues with Haskell language and label "good-first-issue"
fetchGoodFirstIssue :: IO (Either Error (SearchResult Issue))
fetchGoodFirstIssue = searchIssues "language:haskell label:\"good first issue\""