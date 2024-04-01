module StoreSpec
  ( spec
  )
where

import Data.Map qualified as Map
import Database.Schema.Migrations.Migration
import Database.Schema.Migrations.Store
import Test.Hspec
import Prelude

spec :: Spec
spec = do
  describe "validateSingleMigration" $ do
    it "handles an empty map with deps" $ do
      validateSingleMigration emptyMap withDeps
        `shouldBe` [ DependencyReferenceError (mId withDeps) "one"
                   , DependencyReferenceError (mId withDeps) "two"
                   , DependencyReferenceError (mId withDeps) "three"
                   ]

    it "handles an empty map without deps" $ do
      validateSingleMigration emptyMap noDeps `shouldBe` []

    it "handles a partial map with deps" $ do
      validateSingleMigration partialMap withDeps
        `shouldBe` [DependencyReferenceError (mId withDeps) "two"]

    it "handles a full map with deps" $ do
      validateSingleMigration fullMap withDeps `shouldBe` []

    it "handles a full map without deps" $ do
      validateSingleMigration fullMap noDeps `shouldBe` []

  describe "validateMigrationMap" $ do
    it "handles an empty map" $ do
      validateMigrationMap emptyMap `shouldBe` []

    it "map1 example" $ do
      validateMigrationMap map1 `shouldBe` []

    it "map2 example" $ do
      validateMigrationMap map2
        `shouldBe` [DependencyReferenceError (mId m3) "nonexistent"]

    it "map3 example" $ do
      validateMigrationMap map3
        `shouldBe` [ DependencyReferenceError (mId m4) "one"
                   , DependencyReferenceError (mId m4) "two"
                   ]

emptyMap :: MigrationMap
emptyMap = Map.empty

partialMap :: MigrationMap
partialMap =
  Map.fromList
    [ ("one", undefined)
    , ("three", undefined)
    ]

fullMap :: MigrationMap
fullMap =
  Map.fromList
    [ ("one", undefined)
    , ("two", undefined)
    , ("three", undefined)
    ]

withDeps :: Migration
withDeps =
  Migration
    { mTimestamp = undefined
    , mId = "with_deps"
    , mDesc = Just "with dependencies"
    , mApply = ""
    , mRevert = Nothing
    , mDeps = ["one", "two", "three"]
    }

noDeps :: Migration
noDeps =
  Migration
    { mTimestamp = undefined
    , mId = "no_deps"
    , mDesc = Just "no dependencies"
    , mApply = ""
    , mRevert = Nothing
    , mDeps = []
    }

m1 :: Migration
m1 =
  noDeps
    { mId = "m1"
    , mDeps = []
    }

m2 :: Migration
m2 =
  noDeps
    { mId = "m2"
    , mDeps = ["m1"]
    }

m3 :: Migration
m3 =
  noDeps
    { mId = "m3"
    , mDeps = ["nonexistent"]
    }

m4 :: Migration
m4 =
  noDeps
    { mId = "m4"
    , mDeps = ["one", "two"]
    }

map1 :: MigrationMap
map1 =
  Map.fromList
    [ ("m1", m1)
    , ("m2", m2)
    ]

map2 :: MigrationMap
map2 =
  Map.fromList
    [ ("m3", m3)
    ]

map3 :: MigrationMap
map3 =
  Map.fromList
    [ ("m4", m4)
    ]
