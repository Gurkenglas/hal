{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Lens
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

data Group a
  = Singleton a
  | Array (H.HashMap URI a)
  deriving (Show)

instance ToJSON a => ToJSON (Group a) where
  toJSON (Singleton a) = toJSON a
  toJSON (Array as) = toJSON . fmap snd . sortBy (comparing fst) $ H.toList as

type Links = H.HashMap Rel (Group Link)

type Embeds = H.HashMap Rel (Group Representation)

data Representation = Representation
  { _value :: Value
  , _self :: Link
  , _links :: Links
  , _embeds :: Embeds
  } deriving (Show, Generic)

makeLenses ''Representation

instance ToJSON Representation where
  toJSON = view value . condenseEmbeds . condenseLinks

represent :: (HasProfile a, ToJSON a) => a -> URI -> Representation
represent val uri = Representation
  { _value = toObj $ toJSON val
  , _self = link uri val
  , _links = H.empty
  , _embeds = H.empty
  }

linkSingle :: Rel -> Link -> Representation -> Representation
linkSingle rel = over links . H.insert rel . Singleton

linkMulti :: Rel -> Link -> Representation -> Representation
linkMulti rel l = over links $ H.alter f rel
  where f = Just . addToMultiGroup (href l) l

linkList :: Rel -> [Link] -> Representation -> Representation
linkList rel = over links . H.insert rel . Array . H.fromList . map f
  where f l = (href l, l)

embedSingle :: Rel -> Representation -> Representation -> Representation
embedSingle rel = over embeds . H.insert rel . Singleton

embedMulti :: Rel -> Representation -> Representation -> Representation
embedMulti rel a = over embeds $ H.alter f rel
  where f = Just . addToMultiGroup (href $ a^.self) a

embedList :: Rel -> [Representation] -> Representation -> Representation
embedList rel = over embeds . H.insert rel . Array . H.fromList . map f
  where f a = (href $ a^.self, a)


addToMultiGroup :: URI -> a -> Maybe (Group a) -> Group a
addToMultiGroup u a Nothing = Array $ H.singleton u a
addToMultiGroup u a (Just (Array m)) = Array $ H.insert u a m
addToMultiGroup _ _ (Just (Singleton _)) = error "Can’t add to a singleton."

condenseEmbeds :: Representation -> Representation
condenseEmbeds r = case r^.value of
  Object o -> value .~ (Object $ H.insert "_embedded" (toJSON $ r^.embeds) o) $ r
  v -> condenseEmbeds $ value .~ toObj v $ r

condenseLinks :: Representation -> Representation
condenseLinks r = case r^.value of
  Object o -> value .~ (Object $ H.insert "_links" (toJSON $ linksWithSelf r) o) $ r
  v -> condenseLinks $ value .~ toObj v $ r

linksWithSelf :: Representation -> Links
linksWithSelf r = H.insert "self" (Singleton (r^.self)) (r^.links)

toObj :: Value -> Value
toObj o@(Object _) = o
toObj a = object [("self", a)]
