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
          return $ "Filesystem Parsing" ~: test fspTests
      , do
          fsTests <- FilesystemTest.tests
          return $ "Filesystem general" ~: test fsTests
      , do
          linTests <- LinearMigrationsTest.tests
          return $ "Linear migrations" ~: test linTests
      , do
          cfgTests <- ConfigurationTest.tests
          return $ "Configuration tests" ~: test cfgTests
      ]
  return $
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
