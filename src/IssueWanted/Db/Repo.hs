{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module IssueWanted.Db.Repo where

import Squeal.PostgreSQL ((:::), NullityType (..), PGType (..), (:=>))
import Squeal.PostgreSQL.Schema ( ColumnConstraint(..), SchemumType(..), TableConstraint(..))

type Schema =
    '[ "repos"  ::: 'Table ( -- all Haskell repos
        '[ "pk_repo" ::: 'PrimaryKey '["repoId"] ] :=>
        '[ "repoId"       ::: 'NoDef :=> 'NotNull 'PGint4
         , "repoUrl"      ::: 'NoDef :=> 'NotNull 'PGtext
         , "repoName"     ::: 'Def   :=> 'NotNull 'PGtext
         , "repoOwner"    ::: 'Def   :=> 'NotNull 'PGtext
         , "repoIssueHW"  ::: 'Def   :=> 'NotNull 'PGbool -- Repo has issues labeled "HelpWanted"
         , "repoIssueGFI" ::: 'Def   :=> 'NotNull 'PGbool -- Repo has issues labeled "Good First Issue"
        ])
    , "repoIssueGFI" ::: 'Table ( -- all Haskell issues labeled "Good First Issue"
        '[ "pk_gfi"     ::: 'PrimaryKey '["gfiUrl"]
         , "fk_repo_id" ::: 'ForeignKey '["gfiRepoId"] "repos" '["repoId"]
         ] :=>
        '[ "gfiTitle"  ::: 'Def   :=> 'NotNull ' PGtext
         , "gfiUrl"    ::: 'NoDef :=> 'NotNull 'PGtext
         , "gfiRepoId" ::: 'NoDef :=> 'NotNull 'PGint4
        ])

    , "repoIssueHW" ::: 'Table ( -- all Haskell issues labeled "Help Wanted"
        '[ "pk_hw"      ::: 'PrimaryKey '["hwUrl"]
         , "fk_repo_id" ::: 'ForeignKey '["hwRepoId"] "repos" '["repoId"]
         ] :=>
        '[ "hwTitle"  ::: 'Def   :=> 'NotNull 'PGtext
         , "hwUrl"    ::: 'NoDef :=> 'NotNull 'PGtext
         , "hwRepoId" ::: 'NoDef :=> 'NotNull 'PGint4
        ])
    ]
