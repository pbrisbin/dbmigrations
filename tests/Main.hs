module Main
  ( main
  )
where

import Prelude

import ConfigurationTest qualified
import CycleDetectionTest qualified
import DependencyTest qualified
import FilesystemParseTest qualified
import FilesystemSerializeTest qualified
import FilesystemTest qualified
import LinearMigrationsTest qualified
import MigrationsTest qualified
import StoreTest qualified
import System.Exit
import System.IO (stderr)
import Test.HUnit

loadTests :: IO [Test]
loadTests = do
  ioTests <-
    sequence
      [ do
          fspTests <- FilesystemParseTest.tests
          pure $ "Filesystem Parsing" ~: test fspTests
      , do
          fsTests <- FilesystemTest.tests
          pure $ "Filesystem general" ~: test fsTests
      , do
          linTests <- LinearMigrationsTest.tests
          pure $ "Linear migrations" ~: test linTests
      , do
          cfgTests <- ConfigurationTest.tests
          pure $ "Configuration tests" ~: test cfgTests
      ]
  pure $
    concat
      [ ioTests
      , DependencyTest.tests
      , FilesystemSerializeTest.tests
      , MigrationsTest.tests
      , CycleDetectionTest.tests
      , StoreTest.tests
      ]

main :: IO ()
main = do
  tests <- loadTests
  (testResults, _) <- runTestText (putTextToHandle stderr False) $ test tests
  if errors testResults + failures testResults > 0
    then exitFailure
    else exitSuccess
