{-# LANGUAGE OverloadedStrings #-}

module IssueWanted.Search where

import GitHub (Error, SearchResult, Repo)
import GitHub.Endpoints.Search (searchRepos)


fetchHaskellRepos :: IO (Either Error (SearchResult Repo))
fetchHaskellRepos = searchRepos "https://api.github.com/search/repositories?q=language:haskell&order=desc"
