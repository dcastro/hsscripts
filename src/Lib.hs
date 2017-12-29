{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Process (readCreateProcessWithExitCode, proc, cwd)
import GHC.Generics (Generic)
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

stackQuery :: IO [Component]
stackQuery = 
  do
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode process ""
    
    case exitCode of
      ExitFailure _ -> die ("`stack query` failed:\n" ++ stderr)
      ExitSuccess   -> pure ()

    case decodeComponents stdout of
      Right cs -> pure cs
      Left err -> die ("Could not parse response from `stack query`:\n" ++ err)
  where
    process = (proc stackDir ["query"]) { cwd = Just wd }


decodeComponents :: String -> Either String [Component]
decodeComponents s = 
  let resultOpt = Y.decodeEither (BS.pack s) :: Either String Result
  in  components . locals <$> resultOpt

newtype Result = Result
  { locals :: Locals
  }
  deriving (Generic, Show)

newtype Locals = Locals { components :: [Component] }
  deriving Show

data Component = Component
  { name :: Text
  , path :: Text
  , version :: Text
  }
  deriving Show

instance FromJSON Result

instance FromJSON Locals where
  parseJSON (Object o) = 
    let pairs = HM.toList o
        parseComponent (k, Object v) = Component k <$> v .: "path" <*> v .: "version"
    in  Locals <$> traverse parseComponent pairs
  parseJSON _ = fail "Expected Object for Locals value"




