{-# LANGUAGE TemplateHaskell #-}

module Common
  ( TestDependable (..)
  , repoRoot
  , testFile
  , satisfies
  , (.&&.)
  )
where

import Prelude

import CommonTH
import Data.Text (Text)
import Database.Schema.Migrations.Dependencies (Dependable (..))
import Language.Haskell.TH.Syntax (lift)
import System.FilePath ((</>))
import Test.HUnit

repoRoot :: FilePath
repoRoot = $(getRepoRoot >>= lift)

testFile :: FilePath -> FilePath
testFile fp = repoRoot </> "tests" </> fp

instance Dependable TestDependable where
  depId = tdId
  depsOf = tdDeps

data TestDependable = TD
  { tdId :: Text
  , tdDeps :: [Text]
  }
  deriving stock (Show, Eq, Ord)

satisfies :: String -> a -> (a -> Bool) -> IO Test
satisfies m v f = return $ TestCase $ assertBool m (f v)

(.&&.) :: Test -> Test -> Test
(TestList xs) .&&. (TestList ys) = TestList (xs ++ ys)
(TestList xs) .&&. y = TestList (xs ++ [y])
x .&&. (TestList ys) = TestList (x : ys)
a .&&. b = TestList [a, b]
infixl 0 .&&.
