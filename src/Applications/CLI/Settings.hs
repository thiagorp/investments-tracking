{-# LANGUAGE DeriveGeneric #-}

module Applications.CLI.Settings where

import RIO (Either, FilePath, Generic, IO, String)
import System.Envy

newtype Settings = Settings
  {dataDir :: FilePath}
  deriving (Generic)

instance FromEnv Settings

loadSettings :: IO (Either String Settings)
loadSettings = decodeEnv