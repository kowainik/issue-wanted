module IW.Effects.Cabal
       ( getCabalCategories

       -- * Internals
       , getCabalCategoriesImpl
       , repoCabalUrl
       ) where

import Data.Text (splitOn, strip)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parsec (parseGenericPackageDescriptionMaybe)

import IW.App (App)
import IW.Core.Repo (RepoOwner (..), RepoName (..), Category (..))
import IW.Core.Url (Url (..))
import IW.Effects.Download (MonadDownload (..))


class Monad m => MonadCabal m where
    getCabalCategories :: RepoOwner -> RepoName -> m [Category]

instance MonadCabal App where
    getCabalCategories = getCabalCategoriesImpl

getCabalCategoriesImpl :: MonadDownload m => RepoOwner -> RepoName -> m [Category]
getCabalCategoriesImpl repoOwner repoName = do
    cabalFile <- downloadFile $ repoCabalUrl repoOwner repoName 
    pure $ case parseGenericPackageDescriptionMaybe cabalFile of
        Nothing -> []
        Just genPkgDescr -> categoryNames genPkgDescr

repoCabalUrl :: RepoOwner -> RepoName -> Url
repoCabalUrl (RepoOwner repoOwner) (RepoName repoName) = Url $
    "https://raw.githubusercontent.com/" 
    <> repoOwner 
    <> "/" 
    <> repoName
    <> "/master/"
    <> repoName
    <> ".cabal" 

categoryNames :: GenericPackageDescription -> [Category]
categoryNames genPkgDescr = Category . strip <$> splitCategories genPkgDescr
  where
    splitCategories :: GenericPackageDescription -> [Text]
    splitCategories = splitOnNonEmptyText "," . toText . category . packageDescription

    splitOnNonEmptyText :: Text -> Text -> [Text]
    splitOnNonEmptyText _ ""       = []
    splitOnNonEmptyText delim text = splitOn delim text
