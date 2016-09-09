module Data.HalSpec (spec) where

import Data.Hal

import Data.Aeson
import Data.Aeson.Pointer
import Data.Text
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

data Ex = Ex { foo :: Text
             , bar :: Integer
             } deriving (Eq, Generic, Show)
instance FromJSON Ex
instance ToJSON Ex
instance Profile Ex where
  profileOf = const $ Just "ex"

spec :: Spec
spec = do
  describe "object with self rel" $ do
    let ex = Ex "baz" 42
        json = toJSON $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" json `shouldBe` Right "baz"
      pointTo "/bar" json `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" json `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" json `shouldBe` Right "ex"

toPtr :: Text -> Pointer
toPtr t = let Right p = unescape t in p

pointTo :: Text -> Value -> Either ResolutionError Value
pointTo = resolve . toPtr
