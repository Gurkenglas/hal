{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hal
  ( Link(..)
  , Representation
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

type Links = HashMap Text Link

data Representation = Representation
  { self :: Value
  , selfRel :: Link
  , links :: Links
  } deriving (Show, Generic)

class Profile a where
  profileOf :: a -> Maybe Text

instance ToJSON Representation where
  toJSON = condenseLinks

condenseLinks :: Representation -> Value
condenseLinks Representation{..} = case self of
  Object o -> Object $ insert "_links" (toJSON links') o
    where links' = addLink selfRel "self" links
  _ -> error "blargh"

represent :: (Profile a, ToJSON a) => a -> Text -> Representation
represent val uri = Representation
  { self = toJSON val
  , selfRel = Link uri $ profileOf val
  , links = empty
  }

linkTo :: Link -> Text -> Representation -> Representation
linkTo l rel rep = rep { links = addLink l rel $ links rep }

addLink :: Link -> Text -> Links -> Links
addLink l rel ls = insert rel l ls
