{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hal where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)

data Link = Link { href    :: Text
                 , profile :: Maybe Text
                 } deriving (Show, Generic)

data Representation a = Representation (HashMap Text Link) a
                        deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

class Profile a where
  profileOf :: a -> Maybe Text

instance ToJSON a => ToJSON (Representation a) where
  toJSON (Representation links a) =
    let (Object jl) = toJSON a
    in object $ (toList jl) ++ ["_links"  .= links]

represent :: (Profile a, ToJSON a) => a -> Text -> Representation a
represent val href' = Representation links val
  where links = singleton "self" . Link href' $ profileOf val

linkTo :: Text -> Link -> Representation a -> Representation a
linkTo rel link (Representation links val) = Representation links' val
  where links' = insert rel link links
