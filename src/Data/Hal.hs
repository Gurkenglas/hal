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
import Data.Aeson hiding (Array)
import Data.HashMap.Strict
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

type URI = Text

type Rel = Text

data Link = Link
  { href    :: URI
  } deriving (Show, Generic)

instance FromJSON Link
instance ToJSON Link

link :: URI -> Link
link = Link

data Representation = Representation
  { value :: Value
  , self :: Link
  , links :: Links
  , embeds :: Embeds
  } deriving (Show, Generic)

instance ToJSON Representation where
  toJSON = value . condenseEmbeds . condenseLinks

represent :: ToJSON a => a -> URI -> Representation
represent val uri = Representation
  { value = toObj $ toJSON val
  , self = Link uri
  , links = empty
  , embeds = empty
  }

linkSingle :: Rel -> Link -> Representation -> Representation
linkSingle rel l rep = rep { links = insert rel (Singleton l) $ links rep }

linkMulti :: Rel -> Link -> Representation -> Representation
linkMulti rel l rep = rep { links = alter f rel $ links rep }
  where f = Just . addToMultiGroup (href l) l

embedSingle :: Rel -> Representation -> Representation -> Representation
embedSingle rel a rep = rep { embeds = insert rel (Singleton a) $ embeds rep }

embedMulti :: Rel -> Representation -> Representation -> Representation
embedMulti rel a rep = rep { embeds = alter f rel $ embeds rep }
  where f = Just . addToMultiGroup (href $ self a) a


type Links = HashMap Rel (Group Link)

type Embeds = HashMap Rel (Group Representation)

data Group a
  = Singleton a
  | Array (HashMap URI a)
  deriving (Show)

instance ToJSON a => ToJSON (Group a) where
  toJSON (Singleton a) = toJSON a
  toJSON (Array as) = toJSON . fmap snd . sortBy (comparing fst) $ toList as

addToMultiGroup :: URI -> a -> Maybe (Group a) -> Group a
addToMultiGroup u a Nothing = Array $ singleton u a
addToMultiGroup u a (Just (Array m)) = Array $ insert u a m
addToMultiGroup _ _ (Just (Singleton _)) = error "Can’t add to a singleton."

condenseEmbeds :: Representation -> Representation
condenseEmbeds r@Representation{..} = case value of
  Object o -> r { value = Object $ insert "_embedded" (toJSON embeds) o }
  v -> condenseEmbeds $ r { value = toObj v }

condenseLinks :: Representation -> Representation
condenseLinks r@Representation{..} = case value of
  Object o -> r { value = Object $ insert "_links" (toJSON links') o }
    where links' = insert "self" (Singleton self) links
  v -> condenseLinks $ r { value = toObj v }

toObj :: Value -> Value
toObj o@(Object _) = o
toObj a = object [("self", a)]
