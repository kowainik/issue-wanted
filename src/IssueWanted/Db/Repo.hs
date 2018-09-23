{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module IssueWanted.Db.Repo where

import Squeal.PostgreSQL ((:::), (:=>), ColumnConstraint (..), Definition, NP (..),
                          NullityType (..), OnDeleteClause (..), OnUpdateClause (..), PGType (..),
                          SchemumType (..), TableConstraint (..), as, createTable, foreignKey, int,
                          notNullable, nullable, primaryKey, serial, text)

import qualified Squeal.PostgreSQL as SP


type Schema =
    -- all Haskell repos
    '[ "repos"  ::: 'Table (
        '[ "pk_repo" ::: 'PrimaryKey '["repoId"] ] :=>
        '[ "repoId"       ::: 'Def :=> 'NotNull 'PGint4
         , "repoName"     ::: 'NoDef :=> 'NotNull 'PGtext
         , "repoUrl"      ::: 'NoDef :=> 'NotNull 'PGtext
         , "repoOwner"    ::: 'NoDef :=> 'NotNull 'PGtext
         , "repoIssueHW"  ::: 'NoDef :=> 'NotNull 'PGbool -- Repo has issues labeled "HelpWanted"
         , "repoIssueGFI" ::: 'NoDef :=> 'NotNull 'PGbool -- Repo has issues labeled "Good First Issue"
        ])
    -- all Haskell issues labeled "Good First Issue"
    , "repoIssueGFI" ::: 'Table (
        '[ "pk_gfi"     ::: 'PrimaryKey '["gfiId"]
         , "fk_repo_id" ::: 'ForeignKey '["gfiRepoId"] "repos" '["repoId"]
         ] :=>
        '[ "gfiId"     ::: 'Def   :=> 'NotNull 'PGint4
         , "gfiRepoId" ::: 'NoDef :=> 'NotNull 'PGint4
         , "gfiTitle"  ::: 'NoDef   :=> 'Null 'PGtext
         , "gfiUrl"    ::: 'NoDef   :=> 'Null 'PGtext
        ])
    -- all Haskell issues labeled "Help Wanted"
    , "repoIssueHW" ::: 'Table (
        '[ "pk_hw"      ::: 'PrimaryKey '["hwId"]
         , "fk_repo_id" ::: 'ForeignKey '["hwRepoId"] "repos" '["repoId"]
         ] :=>
        '[ "hwId"     ::: 'Def   :=> 'NotNull 'PGint4
         , "hwRepoId" ::: 'NoDef :=> 'NotNull 'PGint4
         , "hwTitle"  ::: 'NoDef :=> 'Null 'PGtext
         , "hwUrl"    ::: 'NoDef :=> 'Null 'PGtext
        ])
    ]

setup :: Definition '[] Schema
setup =
    createTable #repos
        ( serial `as` #repoId
        :* (text & notNullable) `as` #repoName
        :* (text & notNullable) `as` #repoUrl
        :* (text & notNullable) `as` #repoOwner
        :* (SP.bool & notNullable) `as` #repoIssueHW
        :* (SP.bool & notNullable) `as` #repoIssueGFI
        )
        ( primaryKey #repoId `as` #pk_repo ) >>>
    createTable #repoIssueGFI
        ( serial `as` #gfiId
        :* (int & notNullable) `as` #gfiRepoId
        :* (text & nullable) `as` #gfiTitle
        :* (text & nullable) `as` #gfiUrl
        )
        ( primaryKey #gfiId `as` #pk_gfi :*
          foreignKey #gfiRepoId #repos #repoId
            OnDeleteCascade OnUpdateCascade `as` #fk_repo_id ) >>>
    createTable #repoIssueHW
        ( serial `as` #hwId
        :* (int & notNullable) `as` #hwRepoId
        :* (text & nullable) `as` #hwTitle
        :* (text & nullable) `as` #hwUrl
        )
        ( primaryKey #hwId `as` #pk_hw :*
          foreignKey #hwRepoId #repos #repoId
            OnDeleteCascade OnUpdateCascade `as` #fk_repo_id )

-- > printSQL setup
-- CREATE TABLE "repos"
--     ( "repoId" serial
--     , "repoName" text NOT NULL
--     , "repoUrl" text NOT NULL
--     , "repoOwner" text NOT NULL
--     , "repoIssueHW" bool NOT NULL
--     , "repoIssueGFI" bool NOT NULL
--     , CONSTRAINT "pk_repo" PRIMARY KEY ("repoId")
--     );
-- CREATE TABLE "repoIssueGFI"
--     ( "gfiId" serial
--     , "gfiRepoId" int NOT NULL
--     , "gfiTitle" text NULL
--     , "gfiUrl" text NULL
--     , CONSTRAINT "pk_gfi" PRIMARY KEY ("gfiId")
--     , CONSTRAINT "fk_repo_id" FOREIGN KEY ("gfiRepoId") REFERENCES "repos" ("repoId") ON DELETE CASCADE ON UPDATE CASCADE);
-- CREATE TABLE "repoIssueHW"
--     ("hwId" serial
--     , "hwRepoId" int NOT NULL
--     , "hwTitle" text NULL
--     , "hwUrl" text NULL
--     , CONSTRAINT "pk_hw" PRIMARY KEY ("hwId")
--     , CONSTRAINT "fk_repo_id" FOREIGN KEY ("hwRepoId") REFERENCES "repos" ("repoId") ON DELETE CASCADE ON UPDATE CASCADE);

