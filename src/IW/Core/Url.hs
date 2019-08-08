module IW.Core.Url
       ( Url (..)
       ) where


newtype Url = Url
    { unUrl :: Text
    } deriving stock   (Generic)
      deriving newtype (Eq, Ord, Show, FromField, ToField, ToJSON)
