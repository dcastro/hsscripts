{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Process (readCreateProcessWithExitCode, proc, cwd)
import Data.Yaml (FromJSON, Value(..), (.:))
import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import System.Exit (die, ExitCode(..))

stackDir, wd :: String
stackDir = "/Users/dcastro/.local/bin/stack"
-- wd = "/Users/dcastro/Dropbox/Projects/Haskell/halive"
wd = "/Users/dcastro/Dropbox/Projects/Haskell/haskell-ide-engine"

stackQuery :: IO [CabalFile]
stackQuery = 
  do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    
    case exitCode of
      ExitFailure _ -> die ("`stack query` failed:\n" ++ stderr)
      ExitSuccess   -> pure ()

    case Y.decodeEither (BS.pack stdout) of
      Right (StackQuery files)  -> pure files
      Left err                  -> die ("Could not parse response from `stack query`:\n" ++ err)
  where
    process = (proc stackDir ["query"]) { cwd = Just wd }

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

