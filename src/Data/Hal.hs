{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Hal
  ( Link(..)
  , Representation(..)
  , Profile(..)
  , represent
  , linkTo
  ) where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.Text (Text)

data Link = Link
  { href    :: Text
  , profile :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

data Representation = Representation
  { links :: HashMap Text Link
  , self :: Value
  } deriving (Show, Generic)

class Profile a where
  profileOf :: a -> Maybe Text

instance ToJSON Representation where
  toJSON rep = let (Object jl) = self rep
               in object $ (toList jl) ++ ["_links"  .= links rep]

represent :: (Profile a, ToJSON a) => a -> Text -> Representation
represent val href' = Representation ls $ toJSON val
  where ls = singleton "self" . Link href' $ profileOf val

linkTo :: Link -> Text -> Representation -> Representation
linkTo l rel rep = rep { links = insert rel l $ links rep }
