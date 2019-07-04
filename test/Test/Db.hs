module Test.Db 
    ( dbSpecs
    ) where

import IW.App (AppEnv)
import IW.Db.Issue (getIssues)

import Test.Assert (equals)
import Test.Common
import Test.Hspec (Spec, describe, it)


dbSpecs :: AppEnv -> Spec
dbSpecs = joinSpecs "DB"
    [ getIssuesSpec
    ]

getIssuesSpec :: AppEnv -> Spec
getIssuesSpec env = describe "getIssues function" $ do
    it "should return a list of length 2" $
        env & (length <$> getIssues) `equals` 2
