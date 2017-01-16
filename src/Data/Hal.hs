{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Hal
  ( Rel
  , URI
  , HasProfile(..)
  , Link
  , link
  , Condensible
  , Representation
  , represent
  , BareRepresentation
  , bare
  , linkSingle
  , linkMulti
  , linkList
  , embedSingle
  , embedMulti
  , embedList
  ) where

import GHC.Generics
import Control.Lens hiding ((.=))
import Data.Aeson hiding (Array)
import qualified Data.HashMap.Strict as H
import Data.List (sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Text (Text)

type URI = Text

type Rel = Text

class HasProfile a where
  profileOf :: a -> Maybe URI

data Link = Link
  { _href    :: URI
  , _profile :: Maybe Text
  } deriving (Show, Generic)

makeLenses ''Link

instance ToJSON Link where
  toJSON (Link h p) =
    object $ catMaybes $ [ Just ("href" .= h)
                         , ("profile" .=) <$> p
                         ]

link :: HasProfile a => URI -> a -> Link
link uri a = Link
  { _href = uri
  , _profile = profileOf a
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

class Condensible a where
  value :: Simple Lens a Value
  links :: Simple Lens a Links
  linksWithSelf :: a -> Links
  embeds :: Simple Lens a Embeds

data Representation = Representation
  { _rvalue :: Value
  , _self :: Link
  , _rlinks :: Links
  , _rembeds :: Embeds
  } deriving (Show, Generic)

makeLenses ''Representation

data BareRepresentation = BareRepresentation
  { _bvalue :: Value
  , _blinks :: Links
  , _bembeds :: Embeds
  } deriving (Show, Generic)

makeLenses ''BareRepresentation

instance Condensible Representation where
  value = rvalue
  links = rlinks
  linksWithSelf r = H.insert "self" (Singleton (r^.self)) (r^.rlinks)
  embeds = rembeds

instance ToJSON Representation where
  toJSON = view rvalue . condenseEmbeds . condenseLinks

represent :: (HasProfile a, ToJSON a) => a -> URI -> Representation
represent val uri = Representation
  { _rvalue = toObj $ toJSON val
  , _self = link uri val
  , _rlinks = H.empty
  , _rembeds = H.empty
  }

instance Condensible BareRepresentation where
  value = bvalue
  links = blinks
  linksWithSelf = view links
  embeds = bembeds

instance ToJSON BareRepresentation where
  toJSON = view bvalue . condenseEmbeds . condenseLinks

bare :: ToJSON a => a -> BareRepresentation
bare val = BareRepresentation
  { _bvalue = toObj $ toJSON val
  , _blinks = H.empty
  , _bembeds = H.empty
  }

linkSingle :: Condensible a => Rel -> Link -> a -> a
linkSingle rel = over links . H.insert rel . Singleton

linkMulti :: Condensible a => Rel -> Link -> a -> a
linkMulti rel l = over links $ H.alter f rel
  where f = Just . addToMultiGroup (l^.href) l

linkList :: Condensible a => Rel -> [Link] -> a -> a
linkList rel = over links . H.insert rel . Array . H.fromList . map f
  where f l = (l^.href, l)

embedSingle :: Condensible a => Rel -> Representation -> a -> a
embedSingle rel = over embeds . H.insert rel . Singleton

embedMulti :: Condensible a => Rel -> Representation -> a -> a
embedMulti rel a = over embeds $ H.alter f rel
  where f = Just . addToMultiGroup (a^.self.href) a

embedList :: Condensible a => Rel -> [Representation] -> a -> a
embedList rel = over embeds . H.insert rel . Array . H.fromList . map f
  where f a = (a^.self.href, a)


addToMultiGroup :: URI -> a -> Maybe (Group a) -> Group a
addToMultiGroup u a Nothing = Array $ H.singleton u a
addToMultiGroup u a (Just (Array m)) = Array $ H.insert u a m
addToMultiGroup _ _ (Just (Singleton _)) = error "Can’t add to a singleton."

condenseEmbeds :: Condensible a => a -> a
condenseEmbeds r = case r^.value of
  Object o -> value .~ (Object $ H.insert "_embedded" (toJSON $ r^.embeds) o) $ r
  v -> condenseEmbeds $ value .~ toObj v $ r

condenseLinks :: Condensible a => a -> a
condenseLinks r = case r^.value of
  Object o -> value .~ (Object $ H.insert "_links" (toJSON $ linksWithSelf r) o) $ r
  v -> condenseLinks $ value .~ toObj v $ r

toObj :: Value -> Value
toObj o@(Object _) = o
toObj a = object [("self", a)]
