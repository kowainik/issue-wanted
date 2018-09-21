{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module IssueWanted.Db.Repo where

import Squeal.PostgreSQL ((:::), NullityType (..), PGType (..))
import Squeal.PostgreSQL.Schema (ColumnType (..))

import qualified GHC.Generics as GHC


type Schema =
    '[ "repos"  ::: -- all Haskell repos
        '[ "repoId"       ::: 'Required ('NotNull 'PGint4)
         , "repoName"     ::: 'Required ('NotNull 'PGtext)
         , "repoUrl"      ::: 'Required ('NotNull 'PGtext)
         , "repoOwner"    ::: 'Required ('NotNull 'PGtext)
         , "repoIssueHW"  ::: 'Required ('NotNull 'PGbool) -- Repo has issues labeled "HelpWanted"
         , "repoIssueGFI" ::: 'Required ('NotNull 'PGbool) -- Repo has issues labeled "Good First Issue"
        ]
    , "repoIssueGFI" ::: -- all Haskell issues labeled "Good First Issue"
        '[ "gfiTitle"  ::: 'Optional ('NotNull ' PGtext)
         , "gfiUrl"    ::: 'Optional ('NotNull 'PGtext)
         , "gfiRepoId" ::: 'Required ('NotNull 'PGint4)
        ]

    , "repoIssueHW" ::: -- all Haskell issues labeled "Help Wanted"
        '[ "hwTitle"  ::: 'Optional ('NotNull 'PGtext)
         , "hwUrl"    ::: 'Optional ('NotNull 'PGtext)
         , "hwRepoId" ::: 'Required ('NotNull 'PGint4)
        ]
    ]

