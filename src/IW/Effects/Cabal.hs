{- | This module contains the class definitiion of @MonadCabal@ and
an instance of @MonadCabal@ for the @App@ monad. Instances of
@MonadCabal@ have a @getCabalCategories@ action that returns @[Category]@
given a @RepoOwner@ and @RepoName@. It does so by downloading a @.cabal@ file
and parsing the @category@ field of the file.
-}

module IW.Effects.Cabal
       ( MonadCabal (..)

       -- * Internals
       , getCabalCategoriesImpl
       , repoCabalUrl
       ) where

import Data.Text (splitOn, strip)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (ParseResult (..),
                                               parseGenericPackageDescription,
                                               runParseResult)

import IW.App (App (..), WithError)
import IW.Core.Repo (Repo (..), Category (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (MonadDownload (..))


-- | Describes a monad that returns @[Category]@ given a @RepoOwner@ and @RepoName@.
class Monad m => MonadCabal m where
    getCabalCategories :: Repo -> m [Category]

instance MonadCabal App where
    getCabalCategories = getCabalCategoriesImpl

type WithCabal env m = (MonadDownload m, WithLog env m, WithError m)

{- | This function may throw anyone of the errors inherited by the use of @downloadFile@
defined in @IW.Effects.Download@. We are using @parseGenericPackageDescriptionMaybe@
which will return @Nothing@ on an unsuccessful parse.
-}
getCabalCategoriesImpl
    :: forall env m.
       WithCabal env m
    => Repo
    -> m [Category]
getCabalCategoriesImpl Repo{..} = do
    cabalFile <- downloadFile repoCabalUrl
    let result = runParseResult $ parseGenericPackageDescription cabalFile
    log D $ "Cabal file parsed with these warnings: " <> show $ fst result
    case snd result of
        Right err -> throwErr


        -- Nothing -> do
        --     log W $ "Couldn't parse file downloaded from " <> unUrl repoCabalUrl
        --     pure []
        -- Just genPkgDescr -> do
        --     log I $ "Successfully parsed file downloaded from " <> unUrl repoCabalUrl
        --     pure $ categoryNames genPkgDescr

-- | Parses a comma separated @Text@ value to @[Category]@.
categoryNames :: GenericPackageDescription -> [Category]
categoryNames genPkgDescr = Category <$> splitCategories genPkgDescr
  where
    splitCategories :: GenericPackageDescription -> [Text]
    splitCategories = splitAndStrip "," . toText . category . packageDescription

    {- | This function takes a delimeter and a delimeter seperated value,
    and returns a list of @Text@ values stripped of excess whitespace.
    Note that it returns an empty list when an empty delimeter seperated value is
    passed in. This prevents the value @[""]@ from being returned.
    -}
    splitAndStrip :: Text -> Text -> [Text]
    splitAndStrip _ ""       = []
    splitAndStrip delim text = strip <$> splitOn delim text
