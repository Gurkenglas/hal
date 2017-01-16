module Data.HalSpec (spec) where

import Data.Hal

import Data.Aeson
import Data.Aeson.Pointer
import Data.Text
import GHC.Generics
import Test.Hspec
import Test.QuickCheck

data Ex = Ex { foo :: Text
             , bar :: Int
             } deriving (Eq, Generic, Show)
instance FromJSON Ex
instance ToJSON Ex
instance HasProfile Ex where
  profileOf _ex = Just "http://foo.doc/types/ex"
data Icon = Icon deriving (Show)
instance HasProfile Icon where
  profileOf _icon = Just "http://foo.doc/types/icon"

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

  describe "object with one singleton link added" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        l = link "http://static.test/images/ex/1" Icon
        rep = linkSingle "icon" l $ represent ex "http://foo.test/ex/1"
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

  describe "object with one link in an array" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        l = link "http://static.test/images/ex/1" Icon
        rep = linkMulti "icons" l $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "encodes the added link at the given rel in links" $ do
      pointTo "/_links/icons/0/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/1"
      pointTo "/_links/icons/0/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"

  describe "object with two links in the same array" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        l1 = link "http://static.test/images/ex/1" Icon
        l2 = link "http://static.test/images/ex/2" Icon
        rep = linkMulti "icons" l2 $ linkMulti "icons" l1 $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "encodes both links in the same array" $ do
      pointTo "/_links/icons/0/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/1"
      pointTo "/_links/icons/0/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"
      pointTo "/_links/icons/1/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/2"
      pointTo "/_links/icons/1/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"

  describe "object with three links added as a list" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        ls = [ link "http://static.test/images/ex/1" Icon
             , link "http://static.test/images/ex/2" Icon
             , link "http://static.test/images/ex/3" Icon
             ]
        rep = linkList "icons" ls $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "encodes all links in the same array" $ do
      pointTo "/_links/icons/0/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/1"
      pointTo "/_links/icons/0/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"
      pointTo "/_links/icons/1/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/2"
      pointTo "/_links/icons/1/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"
      pointTo "/_links/icons/2/href" (toJSON rep) `shouldBe` Right "http://static.test/images/ex/3"
      pointTo "/_links/icons/2/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/icon"

  describe "object with one singleton embed added" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        embedded = Ex { foo = "quux", bar = 3 }
        rep = embedSingle "ex" (represent embedded "http://foo.test/ex/2")
              $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/ex"
    it "embeds the additional object" $ do
      pointTo "/_embedded/ex/foo" (toJSON rep) `shouldBe` Right "quux"
      pointTo "/_embedded/ex/bar" (toJSON rep) `shouldBe` Right (Number 3)
    it "includes the self rel for the embedded object" $ do
      pointTo "/_embedded/ex/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/2"

  describe "object with one embed in an array" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        embedded = Ex { foo = "quux", bar = 3 }
        rep = embedMulti "xs" (represent embedded "http://foo.test/ex/2")
              $ represent ex "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/ex"
    it "embeds the additional object in an array" $ do
      pointTo "/_embedded/xs/0/foo" (toJSON rep) `shouldBe` Right "quux"
      pointTo "/_embedded/xs/0/bar" (toJSON rep) `shouldBe` Right (Number 3)
    it "includes the self rel for the embedded object" $ do
      pointTo "/_embedded/xs/0/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/2"

  describe "object with two embeds in the same array" $ do
    let ex1 = Ex { foo = "foo", bar = 1 }
        ex2 = Ex { foo = "bar", bar = 2 }
        ex3 = Ex { foo = "baz", bar = 3 }
        rep = embedMulti "xs" (represent ex3 "http://foo.test/ex/3")
              $ embedMulti "xs" (represent ex2 "http://foo.test/ex/2")
              $ represent ex1 "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "foo"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 1)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "gives the object’s profile in the self rel" $ do
      pointTo "/_links/self/profile" (toJSON rep) `shouldBe` Right "http://foo.doc/types/ex"
    it "embeds both additional objects in the same array" $ do
      pointTo "/_embedded/xs/0/foo" (toJSON rep) `shouldBe` Right "bar"
      pointTo "/_embedded/xs/1/foo" (toJSON rep) `shouldBe` Right "baz"

  describe "object with three embeds added as a list" $ do
    let ex1 = Ex { foo = "foo", bar = 1 }
        ex2 = Ex { foo = "bar", bar = 2 }
        ex3 = Ex { foo = "baz", bar = 3 }
        ex4 = Ex { foo = "quux", bar = 4 }
        rep = embedList "xs" [ represent ex2 "http://foo.test/ex/2"
                             , represent ex3 "http://foo.test/ex/3"
                             , represent ex4 "http://foo.test/ex/4"
                             ] $ represent ex1 "http://foo.test/ex/1"
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "foo"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 1)
    it "encodes a link to self at the self rel in links" $ do
      pointTo "/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/1"
    it "encodes all embeds in the same array" $ do
      pointTo "/_embedded/xs/0/foo" (toJSON rep) `shouldBe` Right "bar"
      pointTo "/_embedded/xs/0/bar" (toJSON rep) `shouldBe` Right (Number 2)
      pointTo "/_embedded/xs/1/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/_embedded/xs/1/bar" (toJSON rep) `shouldBe` Right (Number 3)
      pointTo "/_embedded/xs/2/foo" (toJSON rep) `shouldBe` Right "quux"
      pointTo "/_embedded/xs/2/bar" (toJSON rep) `shouldBe` Right (Number 4)

  describe "bare object with one embed in an array" $ do
    let ex = Ex { foo = "baz", bar = 42 }
        embedded = Ex { foo = "quux", bar = 3 }
        rep = embedMulti "xs" (represent embedded "http://foo.test/ex/2")
              $ bare ex
    it "encodes the basic state at the root" $ do
      pointTo "/foo" (toJSON rep) `shouldBe` Right "baz"
      pointTo "/bar" (toJSON rep) `shouldBe` Right (Number 42)
    it "embeds the additional object in an array" $ do
      pointTo "/_embedded/xs/0/foo" (toJSON rep) `shouldBe` Right "quux"
      pointTo "/_embedded/xs/0/bar" (toJSON rep) `shouldBe` Right (Number 3)
    it "includes the self rel for the embedded object" $ do
      pointTo "/_embedded/xs/0/_links/self/href" (toJSON rep) `shouldBe` Right "http://foo.test/ex/2"

toPtr :: Text -> Pointer
toPtr t = let Right p = unescape t in p

pointTo :: Text -> Value -> Either ResolutionError Value
pointTo = resolve . toPtr
