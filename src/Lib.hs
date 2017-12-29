{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}



module Lib where

-- import Stack.Build
-- import Stack.Types.Config
-- import Stack.Runners
-- import Stack.Options.GlobalParser
import System.Process
import Data.Yaml


stackDir, wd :: String
stackDir = "/Users/dcastro/.local/bin/stack"
wd = "/Users/dcastro/Dropbox/Projects/Haskell/haskell-ide-engine"

stackQuery :: IO String
stackQuery = readCreateProcess process ""
  where
    process = (proc stackDir ["query"]) { cwd = Just wd }

-- stackQuery :: IO ()
-- stackQuery = queryCmd [] (globalOptsFromMonoid False mempty)
--   where
--     queryCmd selectors go = withBuildConfig go $ queryBuildInfo $ map T.pack selectors



