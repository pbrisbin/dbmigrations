{-# LANGUAGE OverloadedStrings #-}
module FilesystemTest
    ( tests
    )
where

import Database.Schema.Migrations.Filesystem
import Database.Schema.Migrations.Store ( MigrationStore(..) )

import Test.HUnit
import qualified Data.Set as Set
import Common

tests :: IO [Test]
tests = sequence [getMigrationsTest]

getMigrationsTest :: IO Test
getMigrationsTest = do
  let store = filesystemStore $ FSStore { storePath = testFile "migration_parsing" }
      expected = Set.fromList [ "invalid_field_name"
                              , "invalid_missing_required_fields"
                              , "invalid_syntax"
                              , "invalid_timestamp"
                              , "valid_full"
                              , "valid_no_depends"
                              , "valid_no_desc"
                              , "valid_no_revert"
                              , "valid_no_timestamp"
                              , "valid_with_comments"
                              , "valid_with_comments2"
                              , "valid_with_colon"
                              , "valid_with_multiline_deps"
                              ]
  migrations <- getMigrations store
  return $ expected ~=? Set.fromList migrations
