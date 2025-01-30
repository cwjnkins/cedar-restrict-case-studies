{-# LANGUAGE DeriveDataTypeable #-}

module Config where

import System.Console.CmdArgs

data Config =
    GC
      { numStudents           :: Int
      , numCourses            :: Int
      , maxClassSize          :: Int
      , numAdditionalTeachers :: Int
      , numTAs                :: Int
      , maxCourseAssignments  :: Int
      , seed                  :: Int
      , entityStore           :: FilePath
      , policyStore           :: FilePath
      , logs                  :: FilePath
      }
  | PM
      { numDevs     :: Int
      , numPlanners :: Int
      , numPMs      :: Int
      , numAccts    :: Int
      , numProjs    :: Int
      , maxProjSize :: Int
      , seed        :: Int
      , entityStore :: FilePath
      , policyStore :: FilePath
      , logs        :: FilePath
      }
  deriving (Show, Data, Typeable)

gclass = GC
  { numStudents           = 100 &= help "(Default: 100)"
  , numCourses            = 10  &= help "(Default: 10)"
  , maxClassSize          = 15  &= help "(Default: 15)"
  , numAdditionalTeachers = 9   &= help "(Default: 9)"
  , numTAs                = 6   &= help "(Default: 6)"
  , maxCourseAssignments  = 5   &= help "(Default: 5)"
  , seed                  = 2025
  , entityStore           =    "./assets/gclassroom/entities.json"
                            &= typFile
                            &= help "(Default: ./assets/gclassroom/entities.json)"
  , policyStore           =    "./assets/gclassroom/policies.cedar"
                            &= typFile
                            &= help "(Default: ./assets/gclassroom/policies.cedar)"
  , logs                  =    "./assets/gclassroom/logs.json"
                            &= typFile
                            &= help "(Default: ./assets/gclassroom/logs.json)"
  } &= help "Generate Cedar classroom case study"

projman = PM
  { numDevs = 50
  , numPlanners = 20
  , numPMs = 15
  , numAccts = 15
  , numProjs = 10
  , maxProjSize = 20
  , seed = 2025
  , entityStore = "./assets/project-management/entities.json" &= typFile
  , policyStore = "./assets/project-management/policies.cedar" &= typFile
  , logs = "./assets/project-management/logs.json" &= typFile
  } &= help "Generate Cedar project management case study"

conf =
     modes [gclass, projman]
  &= summary "Generate Cedar case studies"
  &= program "cav2025-cedar-restrict-gclassroom-exe"

