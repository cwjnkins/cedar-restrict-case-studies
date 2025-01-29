{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings #-}

module Lib.IO where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.Char
import Data.Function
import GHC.Generics
import System.Process.Typed

import Lib.CedarFormat
import Lib.Request

data CedarCtxt =
  CedarCtxt
  { exe :: FilePath
  , schema :: Maybe FilePath
  , entities :: FilePath
  , policies :: FilePath
  }

authorize' :: CedarCtxt -> Request' -> IO String
authorize' CedarCtxt{..} Request{..} = do
  (_, out, err) <- readProcess cmd
  return (out & B.toString & filter (not . isSpace))
  where
    cmd :: ProcessConfig () () ()
    cmd = shell $
         exe ++ " authorize "
      ++ "--principal " ++ cedarFormat principal ++ " "
      ++ "--action "    ++ cedarFormat action    ++ " "
      ++ "--resource "  ++ cedarFormat resource  ++ " "
      ++ "--policies "  ++ policies ++ " "
      ++ (case schema of
            Nothing -> ""
            Just sch -> "--schema " ++ sch ++ " ")
      ++ "--entities " ++ entities

data LogEntry =
  LogEntry
    { request :: Request'
    , result  :: String
    } deriving (Generic, Show)

instance ToJSON LogEntry where
