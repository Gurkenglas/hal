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

data Representation a = Representation
  { links :: HashMap Text Link
  , self :: a
  } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

class Profile a where
  profileOf :: a -> Maybe Text

instance ToJSON a => ToJSON (Representation a) where
  toJSON rep = let (Object jl) = toJSON $ self rep
               in object $ (toList jl) ++ ["_links"  .= links rep]

represent :: (Profile a, ToJSON a) => a -> Text -> Representation a
represent val href' = Representation ls val
  where ls = singleton "self" . Link href' $ profileOf val

linkTo :: Link -> Text -> Representation a -> Representation a
linkTo l rel rep = rep { links = insert rel l $ links rep }
