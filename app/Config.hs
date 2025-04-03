{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Config where

import Control.Monad.State.Strict
import Data.Function
import Data.List
import System.Console.CmdArgs

defaultDist :: FilePath
defaultDist = "./dist/"

defaultAssets :: FilePath
defaultAssets = "./assets/"

defaultSchema :: FilePath
defaultSchema = "schema.cedarschema"

defaultPolicies :: FilePath
defaultPolicies = "policies.cedar"

defaultLogs :: FilePath
defaultLogs = "logs.json"

defaultStoreBase :: String
defaultStoreBase = "entities"

type Family a = [a]
type Pools a   = [a]

tabulateFamily :: (Int -> Int -> a) -> [Int] -> Family a
tabulateFamily tab sizes =
  zipWith
    (\ prevFam thisFam -> tab prevFam thisFam)
    (0 : (sizes & init))
    sizes

mapFamily :: (a -> b) -> Family [a] -> Family [b]
mapFamily f = map (map f)

zipFamilyWith :: (a -> b -> c) -> Family [a] -> Family [b] -> Family [c]
zipFamilyWith f = zipWith (zipWith f)

toPools :: Family [a] -> Pools [a]
toPools = tail . scanl (++) []

-- stateFamily :: Family (State s a) -> State s (Family a)
-- stateFamily fam = StateT $ \ s ->
--   let comps = [ runState comp s | comp <- fam ] in
--   pure (comps & map fst, comps & last & snd)

data Config =
    GC
      { size :: [Int]
      -- proportion to `size`
      , student_ratio :: Double
      , teacher_ratio :: Double
      , ta_ratio      :: Double

      -- constant factors
      , max_assignments_per_course :: Int
      , max_student_courseload     :: Int
      , max_teacher_courseload     :: Int
      , max_ta_courseload          :: Int

      , eop :: Int

      , seed                  :: Int
      , entity_store_basename :: String
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

entityStoreFP :: Config -> Family FilePath
entityStoreFP GC{..} =
  [ defaultDist ++ "GClassroom/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- size ]
entityStoreFP conf = []

gclass = GC
  { size          = []
  , student_ratio = 1.0 &= help "Student body to `size` ratio"
  , teacher_ratio = 0.1 &= help "Teacher to `size` ratio"
  , ta_ratio      = 0.1 &= help "TA to `size` ratio"

  , max_assignments_per_course = 3
  , max_student_courseload     = 5 &= help "Max courses in which a student enrolls"
  , max_teacher_courseload     = 2 &= help "Max courses run by a teacher"
  , max_ta_courseload          = 2 &= help "Max courses for a TA"

  , eop = 0 &= help "Percentage of exercised overprivileges (Default: 0)"

  , seed                  = 2025
  , entity_store_basename =    defaultStoreBase
                            &= typFile
  , policyStore           =    (defaultAssets ++ "GClassroom/" ++ defaultPolicies)
                            &= typFile
  , logs                  =    (defaultAssets ++ "GClassroom/" ++ defaultLogs)
                            &= typFile
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

  , entityStore = (defaultDist ++ "ProjMan/" ++ defaultStoreBase ++ ".json") -- "./assets/project-management/entities.json" &= typFile
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

preprocess :: Config -> Config
preprocess conf@GC{..} =
  conf { size = size & map (max 1) & sort & nub }
preprocess conf = conf
