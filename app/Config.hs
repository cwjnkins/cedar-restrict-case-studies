{-# LANGUAGE DeriveDataTypeable #-}

module Config where

import Data.Function
import System.Console.CmdArgs

data Config =
    GC
      { numStudents           :: Int
      , numCourses            :: Int
      , maxCourseLoad         :: Int
      , numAdditionalTeachers :: Int
      , numTAs                :: Int
      , maxCourseAssignments  :: Int

      , exercisedOverprivilege :: Int

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

      , exercisedOverprivilege :: Int

      , seed        :: Int
      , entityStore :: FilePath
      , policyStore :: FilePath
      , logs        :: FilePath
      }
  | HC
    { numPC         :: Int
    , numAreas      :: Int
    , numPCChairs   :: Int
    , numNonPC      :: Int
    , maxAreaPapers :: Int
    , maxPaperAuthors :: Int
    , maxPaperReviewers :: Int

    , seed          :: Int
    , entityStore   :: FilePath
    , policyStore   :: FilePath
    , logs          :: FilePath
    }
  deriving (Show, Data, Typeable)

gclass = GC
  { numStudents           = 100 &= help "(Default: 100)"
  , numCourses            = 10  &= help "(Default: 10)"
  , maxCourseLoad         = 5   &= help "(Default: 5)"
  , numAdditionalTeachers = 9   &= help "(Default: 9)"
  , numTAs                = 6   &= help "(Default: 6)"
  , maxCourseAssignments  = 5   &= help "(Default: 5)"

  , exercisedOverprivilege = 0 &= help "Percentage of exercised overprivileges (Default: 0)"

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
  { numDevs     = 50
  , numPlanners = 20
  , numPMs      = 15
  , numAccts    = 15
  , numProjs    = 10
  , maxProjSize = 20
  , seed        = 2025

  , exercisedOverprivilege = 0 &= help "Percentage of exercised overprivileges (Default: 0)"

  , entityStore = "./assets/project-management/entities.json" &= typFile
  , policyStore = "./assets/project-management/policies.cedar" &= typFile
  , logs        = "./assets/project-management/logs.json" &= typFile
  } &= help "Generate Cedar project management case study"

hotcrp = HC
  { numPC             = 10 &= help "(Default: 10)"
  , numAreas          = 5  &= help "(Default: 5)"
  , numPCChairs       = 3  &= help "(Default: 3)"
  , numNonPC          = 15 &= help "(Default: 15)"
  , maxAreaPapers     = 5  &= help "(Default: 5)"
  , maxPaperAuthors   = 3  &= help "(Default: 3)"
  , maxPaperReviewers = 5  &= help "(Default: 5)"

  , seed        = 2025
  , entityStore = "./assets/hotcrp/entities.json" &= typFile
  , policyStore = "./assets/hotcrp/policies.cedar" &= typFile
  , logs        = "./assets/hotcrp/logs.json" &= typFile
  } &= help "Generate Cedar HotCRP case study"

conf =
     modes [gclass, projman, hotcrp]
  &= summary "Generate Cedar case studies"
  &= program "cav2025-cedar-restrict-gclassroom-exe"

numExercisedOverPriv :: Int -> Int -> Int
numExercisedOverPriv totOk percEOP =
  ((totOk & fromIntegral) * proportionAdditional) & floor
  where
    percEOPSafe = percEOP & max 0 & min 100

    percOk :: Double
    percOk = (100 - percEOPSafe) & fromIntegral

    proportionAdditional :: Double
    proportionAdditional = (100.0 / percOk) - 1.0
