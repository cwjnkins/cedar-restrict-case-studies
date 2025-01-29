{-# LANGUAGE DeriveDataTypeable #-}

module Config where

import System.Console.CmdArgs

data GCConfig =
  GCConfig
  { numStudents :: Int
  , numCourses :: Int
  , maxClassSize :: Int
  , numAdditionalTeachers :: Int -- there will always be at least 1
  , numTAs :: Int
  , maxCourseAssignments :: Int
  , seed :: Int
  , entityStore :: FilePath
  , policyStore :: FilePath
  , logs :: FilePath
  } deriving (Show, Data, Typeable)

config :: GCConfig
config =
  GCConfig
  { numStudents           = 8
  , numCourses            = 2
  , maxClassSize          = 5
  , numAdditionalTeachers = 1
  , numTAs                = 1
  , maxCourseAssignments  = 5
  , seed                  = 2025
  , entityStore           = "./entities.json"
  , policyStore           = "./policies.cedar"
  , logs                  = "./logs.json"
  } &= summary "Generate Google classroom case study for Restrictor"
    &= program "cav2025-cedar-restrict-gclassroom-exe"
