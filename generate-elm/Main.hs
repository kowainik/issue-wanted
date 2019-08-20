{- | Generates Elm types from Haskell @issue-wanted@ library.

The generated files can be found in the @frontend/src/IW/Core/@ folder.
-}

module Main (main) where

import Elm (defaultSettings, generateElm)

import IW.Core.Issue (Issue)
import IW.Core.Repo (Repo)


type IwTypes =
   '[ Issue
    , Repo
    ]

main :: IO ()
main = generateElm @IwTypes $ defaultSettings "frontend/src" ["IW", "Core"]
