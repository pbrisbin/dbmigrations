-- | This module provides an abstraction for a /migration store/, a
--  facility in which 'Migration's can be stored and from which they
--  can be loaded.  This module also provides functions for taking
--  'Migration's from a store and converting them into the appropriate
--  intermediate types for use with the rest of this library.
module Database.Schema.Migrations.Store
  ( MigrationStore (..)
  , MapValidationError (..)
  , StoreData (..)
  , MigrationMap

    -- * High-level Store API
  , loadMigrations
  , storeMigrations
  , storeLookup

    -- * Miscellaneous Functions
  , depGraphFromMapping
  , validateMigrationMap
  , validateSingleMigration
  , leafMigrations
  )
where

import Prelude

import Control.Monad (mzero)
import Data.Graph.Inductive.Graph (indeg, labNodes)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Database.Schema.Migrations.Dependencies
  ( DependencyGraph (..)
  , depsOf
  , mkDepGraph
  )
import Database.Schema.Migrations.Migration (Migration (..))

-- | A mapping from migration name to 'Migration'.  This is exported
--  for testing purposes, but you'll want to interface with this
--  through the encapsulating 'StoreData' type.
type MigrationMap = Map.Map Text Migration

data StoreData = StoreData
  { storeDataMapping :: MigrationMap
  , storeDataGraph :: DependencyGraph Migration
  }

-- | The type of migration storage facilities. A MigrationStore is a
--  facility in which new migrations can be created, and from which
--  existing migrations can be loaded.
data MigrationStore = MigrationStore
  { loadMigration :: Text -> IO (Either String Migration)
  -- ^ Load a migration from the store.
  , saveMigration :: Migration -> IO ()
  -- ^ Save a migration to the store.
  , getMigrations :: IO [Text]
  -- ^ Return a list of all available migrations'
  -- names.
  , fullMigrationName :: Text -> IO FilePath
  -- ^ Return the full representation of a given
  -- migration name; mostly for filesystem stores,
  -- where the full representation includes the store
  -- path.
  }

-- | A type for types of validation errors for migration maps.
data MapValidationError
  = -- | A migration claims a dependency on a
    -- migration that does not exist.
    DependencyReferenceError Text Text
  | -- | An error was encountered when
    -- constructing the dependency graph for
    -- this store.
    DependencyGraphError String
  | -- | The specified migration is invalid.
    InvalidMigration String
  deriving stock (Eq)

instance Show MapValidationError where
  show (DependencyReferenceError from to) =
    "Migration " <> show from <> " references nonexistent dependency " <> show to
  show (DependencyGraphError msg) =
    "There was an error constructing the dependency graph: " <> msg
  show (InvalidMigration msg) =
    "There was an error loading a migration: " <> msg

-- | A convenience function for extracting the list of 'Migration's
--  extant in the specified 'StoreData'.
storeMigrations :: StoreData -> [Migration]
storeMigrations storeData =
  Map.elems $ storeDataMapping storeData

-- | A convenience function for looking up a 'Migration' by name in the
--  specified 'StoreData'.
storeLookup :: StoreData -> Text -> Maybe Migration
storeLookup storeData migrationName =
  Map.lookup migrationName $ storeDataMapping storeData

-- | Load migrations from the specified 'MigrationStore', validate the
--  loaded migrations, and return errors or a 'MigrationMap' on
--  success.  Generally speaking, this will be the first thing you
--  should call once you have constructed a 'MigrationStore'.
loadMigrations :: MigrationStore -> IO (Either [MapValidationError] StoreData)
loadMigrations store = do
  migrations <- getMigrations store
  loadedWithErrors <- mapM (loadMigration store) migrations

  let
    mMap = Map.fromList $ [(mId e, e) | e <- loaded]
    validationErrors = validateMigrationMap mMap
    (loaded, loadErrors) = sortResults loadedWithErrors ([], [])
    allErrors = validationErrors <> (InvalidMigration <$> loadErrors)

    sortResults [] v = v
    sortResults (Left e : rest) (ms, es) = sortResults rest (ms, e : es)
    sortResults (Right m : rest) (ms, es) = sortResults rest (m : ms, es)

  ( if null allErrors
      then
        ( do
            -- Construct a dependency graph and, if that succeeds, return
            -- StoreData.
            case depGraphFromMapping mMap of
              Left e -> pure $ Left [DependencyGraphError e]
              Right gr ->
                pure $
                  Right
                    StoreData
                      { storeDataMapping = mMap
                      , storeDataGraph = gr
                      }
        )
      else pure $ Left allErrors
    )

-- | Validate a migration map.  Returns zero or more validation errors.
validateMigrationMap :: MigrationMap -> [MapValidationError]
validateMigrationMap mMap = do
  validateSingleMigration mMap . snd =<< Map.toList mMap

-- | Validate a single migration.  Looks up the migration's
--  dependencies in the specified 'MigrationMap' and returns a
--  'MapValidationError' for each one that does not exist in the map.
validateSingleMigration :: MigrationMap -> Migration -> [MapValidationError]
validateSingleMigration mMap m = do
  depId <- depsOf m
  if isJust $ Map.lookup depId mMap
    then mzero
    else pure $ DependencyReferenceError (mId m) depId

-- | Create a 'DependencyGraph' from a 'MigrationMap'; returns Left if
--  the dependency graph cannot be constructed (e.g., due to a
--  dependency cycle) or Right on success.  Generally speaking, you
--  won't want to use this directly; use 'loadMigrations' instead.
depGraphFromMapping :: MigrationMap -> Either String (DependencyGraph Migration)
depGraphFromMapping mapping = mkDepGraph $ Map.elems mapping

-- | Finds migrations that no other migration depends on (effectively finds all
--  vertices with in-degree equal to zero).
leafMigrations :: StoreData -> [Text]
leafMigrations s = [l | (n, l) <- labNodes g, indeg g n == 0]
 where
  g = depGraph $ storeDataGraph s
