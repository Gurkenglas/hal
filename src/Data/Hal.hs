{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hal
  ( Rel
  , URI
  , Link
  , link
  , Representation
  , represent
  , linkSingle
  , linkMulti
  , embedSingle
  , embedMulti
  ) where

import GHC.Generics
import Data.Aeson
import Data.HashMap.Strict
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

represent :: ToJSON a => a -> URI -> Representation
represent val uri = Representation
  { self = toJSON val
  , selfRel = Link uri
  , links = empty
  , embeds = empty
  }

link :: URI -> Link
link = Link

linkSingle :: Rel -> Link -> Representation -> Representation
linkSingle rel l rep = rep { links = insert rel (SingletonLink l) $ links rep }

linkMulti :: Rel -> Link -> Representation -> Representation
linkMulti rel l rep = rep { links = alter f rel $ links rep }
  where f Nothing = Just $ LinkArray $ singleton (href l) l
        f (Just (LinkArray m)) = Just $ LinkArray $ insert (href l) l m
        f (Just (SingletonLink _)) = error "blargh"

embedSingle :: Rel -> Representation -> Representation -> Representation
embedSingle rel a rep = rep { embeds = embeds' }
  where embeds' = insert rel (SingletonEmbed a) $ embeds rep

embedMulti :: Rel -> Representation -> Representation -> Representation
embedMulti rel a rep = rep { embeds = alter f rel $ embeds rep }
  where f Nothing = Just $ EmbedArray $ singleton (href $ selfRel a) a
        f (Just (EmbedArray m)) = Just $ EmbedArray $ insert (href $ selfRel a) a m
        f (Just (SingletonEmbed _)) = error "blargh"

data Representation = Representation
  { self :: Value
  , selfRel :: Link
  , links :: Links
  , embeds :: Embeds
  } deriving (Show, Generic)

instance ToJSON Representation where
  toJSON = self . condenseEmbeds . condenseLinks

type URI = Text

type Rel = Text

data Link = Link
  { href    :: URI
  } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

type Links = HashMap Rel LinkGroup

data LinkGroup
  = SingletonLink Link
  | LinkArray (HashMap URI Link)
  deriving (Show)

instance ToJSON LinkGroup where
  toJSON (SingletonLink l) = toJSON l
  toJSON (LinkArray m) = toJSON $ fmap snd $ sortBy (comparing fst) $ toList m

type Embeds = HashMap Rel EmbedGroup

data EmbedGroup
  = SingletonEmbed Representation
  | EmbedArray (HashMap URI Representation)
  deriving (Show)

instance ToJSON EmbedGroup where
  toJSON (SingletonEmbed r) = toJSON r
  toJSON (EmbedArray m) = toJSON $ fmap snd $ sortBy (comparing fst) $ toList m

condenseEmbeds :: Representation -> Representation
condenseEmbeds r@Representation{..} = case self of
  Object o -> r { self = Object $ insert "_embedded" (toJSON embeds) o }
  _ -> error "blargh"

condenseLinks :: Representation -> Representation
condenseLinks r@Representation{..} = case self of
  Object o -> r { self = Object $ insert "_links" (toJSON links') o }
    where links' = insert "self" (SingletonLink selfRel) links
  _ -> error "blargh"
