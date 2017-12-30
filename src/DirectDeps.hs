#!/usr/bin/env stack
{- stack
  script
  --resolver lts-10.1
  --package Cabal
  --package yaml
  --package process
  --package bytestring
  --package unordered-containers
  --package text
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module DirectDeps where

import System.Process (readCreateProcessWithExitCode, proc, cwd)
import Data.Yaml (FromJSON, Value(..), (.:))
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (die, ExitCode(..))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)

-- "/Users/dcastro/Dropbox/Projects/Haskell/haskell-ide-engine"

main :: IO ()
main = do
  argsOpt <- parseArgs <$> getArgs
  (projPath, stackPath) <- case argsOpt of
                Just args -> pure args
                Nothing -> die usage
  cabalFiles <- stackQuery projPath stackPath
  print cabalFiles
  pure ()

type ProjectPath = String
type StackPath = String

parseArgs :: [String] -> Maybe (ProjectPath, StackPath)
parseArgs args =
  go args (Nothing, Nothing) >>= \case
    (Nothing, _) -> Nothing
    (Just projPath, stackPathOpt) -> Just (projPath, fromMaybe "stack" stackPathOpt)
  where
    go :: [String] -> (Maybe ProjectPath, Maybe StackPath) -> Maybe (Maybe ProjectPath, Maybe StackPath)
    go [] paths = Just paths
    go (stackFlag : stackPath : xs) (projPathOpt, _)
      | stackFlag == "-s" || stackFlag == "--stack-path" = go xs (projPathOpt, Just stackPath)
    go (unrecognizedFlag : _) _
      | "-" `isPrefixOf` unrecognizedFlag  = Nothing
    go (projPath : xs) (_, stackPathOpt) = go xs (Just projPath, stackPathOpt)

usage :: String
usage = "Usage: stack DirectDeps.hs <project-path> [-s|--stack-path <stack-path>]"

stackQuery :: ProjectPath -> StackPath -> IO [CabalFile]
stackQuery projPath stackPath = 
  do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    
    case exitCode of
      ExitFailure _ -> die ("`stack query` failed:\n" ++ stderr)
      ExitSuccess   -> pure ()

    case Y.decodeEither (BS.pack stdout) of
      Right (StackQuery files)  -> pure files
      Left err                  -> die ("Could not parse response from `stack query`:\n" ++ err)
  where
    process = (proc stackPath ["query"]) { cwd = Just projPath }

newtype StackQuery = StackQuery
  { cabalFiles :: [CabalFile]
  }
  deriving Show

data CabalFile = CabalFile
  { name :: Text
  , path :: Text
  }
  deriving Show

instance FromJSON StackQuery where
  parseJSON (Object o) = 
    let parseCabalFile (k, Object v) = CabalFile k <$> v .: "path"
    in
      do 
        Object locals <- o .: "locals"
        files         <- traverse parseCabalFile $ HM.toList locals
        pure $ StackQuery files
  parseJSON _ = fail "Expected Object for Locals value"

