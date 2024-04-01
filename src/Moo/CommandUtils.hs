module Moo.CommandUtils
  ( apply
  , confirmCreation
  , interactiveAskDeps
  , lookupMigration
  , revert
  , withBackend
  , getCurrentTimestamp
  ) where

import Prelude

import Control.Exception (finally)
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (liftIO)
import Data.Foldable (for_)
import Data.List (intercalate, isPrefixOf, sortBy)
import Data.Maybe (fromJust, isJust)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (getCurrentTime)
import Database.Schema.Migrations (migrationsToApply, migrationsToRevert)
import Database.Schema.Migrations.Backend (Backend (..))
import Database.Schema.Migrations.Migration (Migration (..))
import Database.Schema.Migrations.Store
  ( StoreData
  , storeLookup
  , storeMigrations
  )
import Moo.Core
import System.Exit (ExitCode (..), exitWith)
import System.IO
  ( BufferMode (..)
  , hFlush
  , hGetBuffering
  , hSetBuffering
  , stdin
  , stdout
  )

getCurrentTimestamp :: IO Text
getCurrentTimestamp =
  cs . replace ":" "-" . replace " " "_" . take 19 . show <$> getCurrentTime

apply :: Migration -> StoreData -> Backend -> Bool -> IO [Migration]
apply m storeData backend complain = do
  -- Get the list of migrations to apply
  toApply <- migrationsToApply storeData backend m

  -- Apply them
  if null toApply
    then nothingToDo >> pure []
    else mapM_ (applyIt backend) toApply >> pure toApply
 where
  nothingToDo =
    when complain $
      putStrLn . cs $
        "Nothing to do; "
          <> mId m
          <> " already installed."

  applyIt conn it = do
    putStr . cs $ "Applying: " <> mId it <> "... "
    applyMigration conn it
    putStrLn "done."

revert :: Migration -> StoreData -> Backend -> IO [Migration]
revert m storeData backend = do
  -- Get the list of migrations to revert
  toRevert <- liftIO $ migrationsToRevert storeData backend m

  -- Revert them
  if null toRevert
    then nothingToDo >> pure []
    else mapM_ (revertIt backend) toRevert >> pure toRevert
 where
  nothingToDo =
    putStrLn . cs $
      "Nothing to do; "
        <> mId m
        <> " not installed."

  revertIt conn it = do
    putStr . cs $ "Reverting: " <> mId it <> "... "
    revertMigration conn it
    putStrLn "done."

lookupMigration :: StoreData -> Text -> IO Migration
lookupMigration storeData name = do
  let theMigration = storeLookup storeData name
  case theMigration of
    Nothing -> do
      putStrLn . cs $ "No such migration: " <> name
      exitWith (ExitFailure 1)
    Just m' -> pure m'

-- Given an action that needs a database connection, connect to the
-- database using the backend and invoke the action
-- with the connection. Return its result.
withBackend :: (Backend -> IO a) -> AppT a
withBackend act = do
  backend <- asks _appBackend
  liftIO $ act backend `finally` disconnectBackend backend

-- Given a migration name and selected dependencies, get the user's
-- confirmation that a migration should be created.
confirmCreation :: Text -> [Text] -> IO Bool
confirmCreation migrationId deps = do
  putStrLn ""
  putStrLn . cs $ "Confirm: create migration '" <> migrationId <> "'"
  if null deps
    then putStrLn "  (No dependencies)"
    else putStrLn "with dependencies:"
  forM_ deps $ \d -> putStrLn . cs $ "  " <> d
  prompt
    "Are you sure?"
    [ ('y', (True, Nothing))
    , ('n', (False, Nothing))
    ]

-- Prompt the user for a choice, given a prompt and a list of possible
-- choices.  Let the user get help for the available choices, and loop
-- until the user makes a valid choice.
prompt :: Eq a => String -> PromptChoices a -> IO a
prompt _ [] = error "prompt requires a list of choices"
prompt message choiceMap = do
  putStr $ message <> " (" <> choiceStr <> helpChar <> "): "
  hFlush stdout
  c <- unbufferedGetChar
  case lookup c choiceMap of
    Nothing -> do
      when (c /= '\n') $ putStrLn ""
      when (c == 'h') $ putStr $ mkPromptHelp choiceMapWithHelp
      retry
    Just (val, _) -> putStrLn "" >> pure val
 where
  retry = prompt message choiceMap
  choiceStr = intercalate "" $ map (pure . fst) choiceMap
  helpChar = if hasHelp choiceMap then "h" else ""
  choiceMapWithHelp = choiceMap <> [('h', (undefined, Just "this help"))]

-- Given a PromptChoices, build a multi-line help string for those
-- choices using the description information in the choice list.
mkPromptHelp :: PromptChoices a -> String
mkPromptHelp choices =
  intercalate
    ""
    [ [c] <> ": " <> fromJust msg <> "\n"
    | (c, (_, msg)) <- choices
    , isJust msg
    ]

-- Does the specified prompt choice list have any help messages in it?
hasHelp :: PromptChoices a -> Bool
hasHelp = any hasMsg
 where
  hasMsg (_, (_, m)) = isJust m

-- A general type for a set of choices that the user can make at a
-- prompt.
type PromptChoices a = [(Char, (a, Maybe String))]

-- Get an input character in non-buffered mode, then restore the
-- original buffering setting.
unbufferedGetChar :: IO Char
unbufferedGetChar = do
  bufferingMode <- hGetBuffering stdin
  hSetBuffering stdin NoBuffering
  c <- getChar
  hSetBuffering stdin bufferingMode
  pure c

-- The types for choices the user can make when being prompted for
-- dependencies.
data AskDepsChoice = Yes | No | View | Done | Quit
  deriving stock (Eq)

-- Interactively ask the user about which dependencies should be used
-- when creating a new migration.
interactiveAskDeps :: StoreData -> IO [Text]
interactiveAskDeps storeData = do
  -- For each migration in the store, starting with the most recently
  -- added, ask the user if it should be added to a dependency list
  let sorted = sortBy compareTimestamps $ storeMigrations storeData
  interactiveAskDeps' storeData (map mId sorted)
 where
  compareTimestamps m1 m2 = compare (mTimestamp m2) (mTimestamp m1)

-- Recursive function to prompt the user for dependencies and let the
-- user view information about potential dependencies.  Returns a list
-- of migration names which were selected.
interactiveAskDeps' :: StoreData -> [Text] -> IO [Text]
interactiveAskDeps' _ [] = pure []
interactiveAskDeps' storeData (name : rest) = do
  result <- prompt ("Depend on '" <> cs name <> "'?") askDepsChoices
  if result == Done
    then pure []
    else case result of
      Yes -> do
        next <- interactiveAskDeps' storeData rest
        pure $ name : next
      No -> interactiveAskDeps' storeData rest
      View -> do
        -- load migration
        for_ (storeLookup storeData name) $ \m -> do
          -- print out description, timestamp, deps
          when
            (isJust $ mDesc m)
            ( putStrLn . cs $
                "  Description: "
                  <> fromJust (mDesc m)
            )
          putStrLn $ "      Created: " <> show (mTimestamp m)
          unless
            (null $ mDeps m)
            ( putStrLn . cs $
                "  Deps: "
                  <> T.intercalate "\n        " (mDeps m)
            )

        -- ask again
        interactiveAskDeps' storeData (name : rest)
      Quit -> do
        putStrLn "cancelled."
        exitWith (ExitFailure 1)
      Done -> pure []

-- The choices the user can make when being prompted for dependencies.
askDepsChoices :: PromptChoices AskDepsChoice
askDepsChoices =
  [ ('y', (Yes, Just "yes, depend on this migration"))
  , ('n', (No, Just "no, do not depend on this migration"))
  , ('v', (View, Just "view migration details"))
  , ('d', (Done, Just "done, do not ask me about more dependencies"))
  , ('q', (Quit, Just "cancel this operation and quit"))
  ]

-- The following code is vendored from MissingH Data.List.Utils:

-- | Similar to Data.List.span, but performs the test on the entire remaining
-- list instead of just one element.
--
-- @spanList p xs@ is the same as @(takeWhileList p xs, dropWhileList p xs)@
spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([], [])
spanList func list@(x : xs) =
  if func list
    then (x : ys, zs)
    else ([], list)
 where
  (ys, zs) = spanList func xs

-- | Similar to Data.List.break, but performs the test on the entire remaining
-- list instead of just one element.
breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . split old

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
  let (firstline, remainder) = breakList (isPrefixOf delim) str
  in  firstline : case remainder of
        [] -> []
        x ->
          if x == delim
            then [[]]
            else split delim (drop (length delim) x)
