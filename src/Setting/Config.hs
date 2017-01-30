{-# LANGUAGE OverloadedStrings #-}
module Setting.Config where

import Database.Persist.MySQL hiding (get)
import qualified Data.Configurator as C

data ApiCfg = ApiCfg {
    port :: Int
  , dbConfig :: ConnectInfo
  }

parseConfig :: FilePath -> IO ApiCfg
parseConfig cfgFile = do
  cfg <- C.load [C.Required cfgFile]
  port_ <- C.require cfg "port"
  dbHost <- C.require cfg "db.host"
  dbPort <- C.require cfg "db.port"
  dbUser <- C.require cfg "db.user"
  dbPassword <- C.require cfg "db.password"
  dbName <- C.require cfg "db.name"
  let dbConfig_ = defaultConnectInfo {
      connectHost = dbHost
    , connectPort = dbPort
    , connectUser = dbUser
    , connectPassword = dbPassword
    , connectDatabase = dbName
  }
  return (ApiCfg port_ dbConfig_)
