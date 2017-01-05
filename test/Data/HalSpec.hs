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
  profileOf _ex = Just "http://foo.doc/types/ex"

spec :: Spec
spec = do
  describe "object with self rel" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        json = toJSON $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" json `shouldBe` Right "baz"
      pointTo "/bar" json `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" json `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" json `shouldBe` Right "http://foo.doc/types/ex"
  describe "object with one link added" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        link = Link { href = "http://static.test/images/ex/1"
                    , profile = Just "http://foo.doc/types/icon" }
        rep = linkTo link "icon" $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/ex"
    it "encodes the added link at the given rel in links" $ do
      pointTo "/_links/icon/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/1"
      pointTo "/_links/icon/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"

toPtr :: Text -> Pointer
toPtr t = let Right p = unescape t in p

pointTo :: Text -> Value -> Either ResolutionError Value
pointTo = resolve . toPtr
