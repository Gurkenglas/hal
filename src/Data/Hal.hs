{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hal
  ( Link(..)
  , Representation
  , Profile(..)
  , represent
  , linkTo
  , embedSingle
  , embedMulti
  ) where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

type URI = Text

data Link = Link
  { href    :: URI
  , profile :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

type Links = HashMap Text Link

data Representation = Representation
  { self :: Value
  , selfRel :: Link
  , links :: Links
  , embeds :: Embeds
  } deriving (Show, Generic)

class Profile a where
  profileOf :: a -> Maybe URI

instance ToJSON Representation where
  toJSON = self . condenseEmbeds . condenseLinks

condenseEmbeds :: Representation -> Representation
condenseEmbeds r@Representation{..} = case self of
  Object o -> r { self = Object $ insert "_embedded" (toJSON embeds) o }
  _ -> error "blargh"

condenseLinks :: Representation -> Representation
condenseLinks r@Representation{..} = case self of
  Object o -> r { self = Object $ insert "_links" (toJSON links') o }
    where links' = addLink selfRel "self" links
  _ -> error "blargh"

represent :: (Profile a, ToJSON a) => a -> URI -> Representation
represent val uri = Representation
  { self = toJSON val
  , selfRel = Link uri $ profileOf val
  , links = empty
  , embeds = empty
  }

linkTo :: Link -> Text -> Representation -> Representation
linkTo l rel rep = rep { links = addLink l rel $ links rep }

addLink :: Link -> Text -> Links -> Links
addLink l rel ls = insert rel l ls


type Embeds = HashMap Text EmbedGroup

data EmbedGroup
  = SingletonEmbed Representation
  | EmbedArray (HashMap URI Representation)
  deriving (Show)

instance ToJSON EmbedGroup where
  toJSON (SingletonEmbed r) = toJSON r
  toJSON (EmbedArray m) = toJSON $ fmap snd $ sortBy (comparing fst) $ toList m

embedSingle :: Text -> Representation -> Representation -> Representation
embedSingle label a rep = rep { embeds = embeds' }
  where embeds' = insert label (SingletonEmbed a) $ embeds rep

embedMulti :: Text -> Representation -> Representation -> Representation
embedMulti label a rep = rep { embeds = alter f label $ embeds rep }
  where f Nothing = Just $ EmbedArray $ singleton (href $ selfRel a) a
        f (Just (EmbedArray m)) = Just $ EmbedArray $ insert (href $ selfRel a) a m
        f (Just (SingletonEmbed _)) = error "blargh"
