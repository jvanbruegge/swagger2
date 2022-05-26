{-# LANGUAGE CPP #-}
module SpecCommon where

import Data.Aeson
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Foldable       as F
import qualified Data.HashMap.Strict as HashMap

import Data.Text (Text)
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap   as KM
import Data.Aeson.Key
#else
import qualified Data.HashMap.Strict as KM
#endif
import qualified Data.Vector         as Vector

import Test.Hspec

#if MIN_VERSION_aeson(2,0,0)
toHashMapText :: KM.KeyMap v -> HashMap.HashMap Text v
toHashMapText = KM.toHashMapText
#else
toHashMapText :: HashMap.HashMap a b -> HashMap.HashMap a b
toHashMapText = id

toText :: Text -> Text
toText = id
#endif

isSubJSON :: Value -> Value -> Bool
isSubJSON Null _ = True
isSubJSON (Object x) (Object y) = map toText (KM.keys x) == HashMap.keys i && F.and i
  where
    i = HashMap.intersectionWith isSubJSON (toHashMapText x) (toHashMapText y)
isSubJSON (Array xs) (Array ys) = Vector.length xs == Vector.length ys && F.and (Vector.zipWith isSubJSON xs ys)
isSubJSON x y = x == y

(<=>) :: (Eq a, Show a, ToJSON a, FromJSON a) => a -> Value -> Spec
x <=> js = do
  it "encodes correctly" $ do
    toJSON x `shouldBe` js
  it "decodes correctly" $ do
    fromJSON js `shouldBe` Success x
  it "roundtrips: eitherDecode . encode" $ do
    eitherDecode (encode x) `shouldBe` Right x
  it "roundtrips with toJSON" $ do
    eitherDecode (encode $ toJSON x) `shouldBe` Right x
  it "roundtrips with toEncoding" $ do
    eitherDecode (toLazyByteString $ fromEncoding $ toEncoding x) `shouldBe` Right x
