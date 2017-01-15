{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Hal
  ( Rel
  , URI
  , HasProfile(..)
  , Link
  , link
  , Representation
  , represent
  , linkSingle
  , linkMulti
  , linkList
  , embedSingle
  , embedMulti
  , embedList
  ) where

import GHC.Generics
import Data.Aeson hiding (Array)
import qualified Data.HashMap.Strict as H
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)

type URI = Text

type Rel = Text

class HasProfile a where
  profileOf :: a -> Maybe URI

data Link = Link
  { href    :: URI
  , profile :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Link

link :: HasProfile a => URI -> a -> Link
link uri a = Link { href = uri
                  , profile = profileOf a
                  }

data Representation = Representation
  { value :: Value
  , self :: Link
  , links :: Links
  , embeds :: Embeds
  } deriving (Show, Generic)

instance ToJSON Representation where
  toJSON = value . condenseEmbeds . condenseLinks

represent :: (HasProfile a, ToJSON a) => a -> URI -> Representation
represent val uri = Representation
  { value = toObj $ toJSON val
  , self = link uri val
  , links = H.empty
  , embeds = H.empty
  }

linkSingle :: Rel -> Link -> Representation -> Representation
linkSingle rel l rep = rep { links = H.insert rel (Singleton l) $ links rep }

linkMulti :: Rel -> Link -> Representation -> Representation
linkMulti rel l rep = rep { links = H.alter f rel $ links rep }
  where f = Just . addToMultiGroup (href l) l

linkList :: Rel -> [Link] -> Representation -> Representation
linkList rel ls rep = rep { links = H.insert rel (Array ls') $ links rep }
  where ls' = H.fromList $ fmap (\l -> (href l, l)) ls

embedSingle :: Rel -> Representation -> Representation -> Representation
embedSingle rel a rep = rep { embeds = H.insert rel (Singleton a) $ embeds rep }

embedMulti :: Rel -> Representation -> Representation -> Representation
embedMulti rel a rep = rep { embeds = H.alter f rel $ embeds rep }
  where f = Just . addToMultiGroup (href $ self a) a

embedList :: Rel -> [Representation] -> Representation -> Representation
embedList rel as rep = rep { embeds = H.insert rel (Array as') $ embeds rep }
  where as' = H.fromList $ fmap (\a -> (href $ self a, a)) as


type Links = H.HashMap Rel (Group Link)

type Embeds = H.HashMap Rel (Group Representation)

data Group a
  = Singleton a
  | Array (H.HashMap URI a)
  deriving (Show)

instance ToJSON a => ToJSON (Group a) where
  toJSON (Singleton a) = toJSON a
  toJSON (Array as) = toJSON . fmap snd . sortBy (comparing fst) $ H.toList as

addToMultiGroup :: URI -> a -> Maybe (Group a) -> Group a
addToMultiGroup u a Nothing = Array $ H.singleton u a
addToMultiGroup u a (Just (Array m)) = Array $ H.insert u a m
addToMultiGroup _ _ (Just (Singleton _)) = error "Can’t add to a singleton."

condenseEmbeds :: Representation -> Representation
condenseEmbeds r@Representation{..} = case value of
  Object o -> r { value = Object $ H.insert "_embedded" (toJSON embeds) o }
  v -> condenseEmbeds $ r { value = toObj v }

condenseLinks :: Representation -> Representation
condenseLinks r@Representation{..} = case value of
  Object o -> r { value = Object $ H.insert "_links" (toJSON links') o }
    where links' = H.insert "self" (Singleton self) links
  v -> condenseLinks $ r { value = toObj v }

toObj :: Value -> Value
toObj o@(Object _) = o
toObj a = object [("self", a)]
