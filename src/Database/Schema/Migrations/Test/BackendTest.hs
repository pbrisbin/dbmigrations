-- | A test that is not executed as part of this package's test suite but rather
-- acts as a conformance test suit for database specific backend
-- implementations. All backend specific executable packages are expected to
-- have a test suite that runs this test.
--
-- Usage:
--
-- @
-- module MyBackendSpec
--   ( spec
--   )
-- where
--
-- import Database.Schema.Migrations.Test.BackendTest hiding (spec)
-- import Database.Schema.Migrations.Test.BackendTest qualified as BackendTest
-- import MyBackend
-- import Test.Hspec
--
-- instance BackendConnection MyBackendConnection where
--   -- ...
--
-- newConnection :: IO MyBackendConnection
-- newConnection = undefined
--
-- spec :: Spec
-- spec = before newConnection BackendTest.spec
-- @
module Database.Schema.Migrations.Test.BackendTest
  ( BackendConnection (..)
  , spec
  ) where

import Prelude

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Database.Schema.Migrations.Backend (Backend (..), bootstrapIfNecessary)
import Database.Schema.Migrations.Migration (Migration (..), newMigration)
import Test.Hspec

class BackendConnection c where
  supportsTransactionalDDL :: c -> Bool
  withTransaction :: c -> (c -> IO a) -> IO a
  getTables :: c -> IO [ByteString]
  dropTables :: c -> IO ()
  makeBackend :: c -> Backend

spec :: BackendConnection bc => SpecWith bc
spec = do
  it "successfully bootstraps" $ \conn -> do
    -- This should be false pre-bootstrap
    isBootstrapped (makeBackend conn) `shouldReturn` False

    let backend = makeBackend conn
    bs <- getBootstrapMigration backend
    applyMigration backend bs

    -- This should be true now
    isBootstrapped (makeBackend conn) `shouldReturn` True

    getTables conn `shouldReturn` ["installed_migrations"]
    getMigrations backend `shouldReturn` [mId bs]

  it "migrates in a transaction" $ needDDL $ \conn -> do
    backend <- makeBootstrappedBackend conn

    let
      m1 =
        (newMigration "second")
          { mApply = "CREATE TABLE validButTemporary (a int)"
          }
      m2 =
        (newMigration "third")
          { mApply = "INVALID SQL"
          }

    ignoreAny $ withTransaction conn $ \conn' -> do
      let backend' = makeBackend conn'
      applyMigration backend' m1
      applyMigration backend' m2

    -- The failure to apply m2 results in no tables
    pendingWith "Fails and I don't know why"
    getTables conn `shouldReturn` ["installed_migrations"]
    getMigrations backend `shouldReturn` ["root"]

  it "applies migrations" $ needDDL $ \conn -> do
    backend <- makeBootstrappedBackend conn

    let m1 =
          (newMigration "validMigration")
            { mApply = "CREATE TABLE valid1 (a int)"
            }

    withTransaction conn $ \conn' -> do
      applyMigration (makeBackend conn') m1

    getTables conn `shouldReturn` ["installed_migrations", "valid1"]
    getMigrations backend `shouldReturn` ["root", "validMigration"]

  context "revertMigration" $ do
    it "handles failure to revert" $ needDDL $ \conn -> do
      backend <- makeBootstrappedBackend conn

      let
        m1 =
          (newMigration "second")
            { mApply = "CREATE TABLE validRMF (a int)"
            , mRevert = Just "DROP TABLE validRMF"
            }
        m2 =
          (newMigration "third")
            { mApply = "alter table validRMF add column b int"
            , mRevert = Just "INVALID REVERT SQL"
            }

      applyMigration backend m1
      applyMigration backend m2

      installedBeforeRevert <- getMigrations backend
      commitBackend backend

      -- Revert the migrations, ignore exceptions; the revert will fail, but
      -- withTransaction will roll back.
      ignoreAny $ withTransaction conn $ \conn' -> do
        let backend' = makeBackend conn'
        revertMigration backend' m2
        revertMigration backend' m1

      getMigrations backend `shouldReturn` installedBeforeRevert

    it "runs the Revert SQL" $ \conn -> do
      backend <- makeBootstrappedBackend conn

      let
        name = "revertable"
        m1 =
          (newMigration name)
            { mApply = "CREATE TABLE the_test_table (a int)"
            , mRevert = Just "DROP TABLE the_test_table"
            }

      applyMigration backend m1

      installedAfterApply <- getMigrations backend
      installedAfterApply `shouldSatisfy` (name `elem`)

      revertMigration backend m1

      tables <- getTables conn
      tables `shouldNotSatisfy` ("the_test_table" `elem`) -- dropped
      installed <- getMigrations backend
      installed `shouldNotSatisfy` (name `elem`)

    it "removes the migration even if there's no Revert SQL" $ \conn -> do
      backend <- makeBootstrappedBackend conn

      let
        name = "second"
        m1 =
          (newMigration name)
            { mApply = "create table revert_nothing (a int)"
            , mRevert = Nothing
            }

      applyMigration backend m1

      installedAfterApply <- getMigrations backend
      installedAfterApply `shouldSatisfy` (name `elem`)

      revertMigration backend m1

      tables <- getTables conn
      tables `shouldSatisfy` ("revert_nothing" `elem`) -- still here
      installed <- getMigrations backend
      installed `shouldNotSatisfy` (name `elem`)

makeBootstrappedBackend :: BackendConnection bc => bc -> IO Backend
makeBootstrappedBackend conn = do
  let backend = makeBackend conn
  backend <$ bootstrapIfNecessary backend

-- | Wrap a spec that requires transactional DDL and mark it pending if the
-- backend does not support that.
needDDL :: BackendConnection bc => (bc -> Expectation) -> bc -> Expectation
needDDL f conn
  | supportsTransactionalDDL conn = f conn
  | otherwise = pendingWith "Skipping due to lack of Transactional DDL"

ignoreAny :: IO a -> IO ()
ignoreAny act = void act `catch` \(_ :: SomeException) -> pure ()
