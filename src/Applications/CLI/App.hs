{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Applications.CLI.App (App, appSettings, runApp) where

import Applications.CLI.Settings
import RIO

data AppEnv = AppEnv
  { appEnvSettings :: Settings
  , appEnvLogFunc :: LogFunc
  }

instance HasLogFunc AppEnv where
  logFuncL = lens appEnvLogFunc (\x y -> x{appEnvLogFunc = y})

newtype App a = App (RIO AppEnv a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader AppEnv)

appSettings :: App Settings
appSettings = asks appEnvSettings

runApp :: Settings -> App a -> IO a
runApp settings (App m) = do
  logOptions' <- logOptionsHandle stdout True
  let logOptions =
        setLogUseLoc True logOptions'
          & setLogMinLevel LevelDebug
          & setLogVerboseFormat True
          & setLogTerminal True
          & setLogUseTime True
          & setLogUseColor True
  withLogFunc logOptions $ \logFunc -> do
    runRIO (AppEnv settings logFunc) m