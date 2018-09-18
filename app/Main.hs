module Main where

import GitHub (Error, Repo, SearchResult)

import Search (fetchAllHaskellRepos)


main :: IO (Either Error (SearchResult Repo))
main = fetchAllHaskellRepos

