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

defaultLogBase :: FilePath
defaultLogBase = "logs"

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

toPoolsGen :: (a -> a -> a) -> Family a -> Pools a
toPoolsGen = scanl1

-- stateFamily :: Family (State s a) -> State s (Family a)
-- stateFamily fam = StateT $ \ s ->
--   let comps = [ runState comp s | comp <- fam ] in
--   pure (comps & map fst, comps & last & snd)

data Config =
    GC
      { size_start :: Int
      , size_incr  :: Int
      , size_end   :: Int
      , priv_rep_ratio :: Double
      , over_priv_percent :: Int

      , seed                  :: Int
      , policy_store          :: FilePath
      , entity_store_basename :: String
      , log_store_basename    :: String

      -- proportion to `size`
      , student_ratio :: Double
      , teacher_ratio :: Double
      , ta_ratio      :: Double

      -- constant factors
      , max_assignments_per_course :: Int
      , max_student_courseload     :: Int
      , max_teacher_courseload     :: Int
      , max_ta_courseload          :: Int
      }
  | PM
      { size_start :: Int
      , size_incr  :: Int
      , size_end   :: Int
      , priv_rep_ratio :: Double
      , over_priv_percent :: Int

      , seed                  :: Int
      , policy_store          :: FilePath
      , entity_store_basename :: String
      , log_store_basename    :: String

      -- proportion to `size`
      , project_ratio :: Double
      , user_ratio    :: Double

      -- constant factors
      , max_user_projectload :: Int
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

sizes :: Config -> [Int]
sizes GC{..} = [size_start,(size_start+size_incr)..size_end]
sizes PM{..} = [size_start,(size_start+size_incr)..size_end]
sizes _ = []

entityStoreFP :: Config -> Family FilePath
entityStoreFP conf@GC{..} =
  [ defaultDist ++ "GClassroom/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
entityStoreFP conf@PM{..} =
  [ defaultDist ++ "ProjMan/" ++ entity_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
entityStoreFP _ = []

logStoreFP :: Config -> Family FilePath
logStoreFP conf@GC{..} =
  [ defaultDist ++ "GClassroom/" ++ log_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
logStoreFP conf@PM{..} =
  [ defaultDist ++ "ProjMan/" ++ log_store_basename ++ "." ++ show i ++ ".json"
  | i <- conf & sizes ]
logStoreFP conf = []

gclass = GC
  { size_start = 20
  , size_incr  = 10
  , size_end   = 40
  , priv_rep_ratio = 0.4  &= help "Ratio of privilege representation to actual privilege"
  , over_priv_percent = 5 &= help "Percentage of privilege representation that is over privilege"

  , student_ratio = 1.0 &= help "Student body to `size` ratio"
  , teacher_ratio = 0.1 &= help "Teacher to `size` ratio"
  , ta_ratio      = 0.1 &= help "TA to `size` ratio"

  , max_assignments_per_course = 3
  , max_student_courseload     = 5 &= help "Max courses in which a student enrolls"
  , max_teacher_courseload     = 2 &= help "Max courses run by a teacher"
  , max_ta_courseload          = 2 &= help "Max courses for a TA"

  , seed                  = 2025
  , entity_store_basename =    defaultStoreBase
                            &= typFile
  , policy_store          =    (defaultAssets ++ "GClassroom/" ++ defaultPolicies)
                            &= typFile
  , log_store_basename    =    defaultLogBase
  } &= help "Generate Cedar classroom case study"

projman = PM
  { size_start = 20
  , size_incr  = 10
  , size_end   = 40
  , priv_rep_ratio = 0.25 &= help "Ratio of privilege representation to actual privilege"
  , over_priv_percent = 5 &= help "Percentage of privilege representation that is over privilege"

  -- proportion to `size`
  , project_ratio = 0.1 &= help "Ratio of number of projects to `size`"
  , user_ratio    = 1.0 &= help "Ratio of number of users to `size`"

  -- constant factors
  , max_user_projectload = 3 &= help "Max projects for user"

  , seed                  = 2025
  , entity_store_basename =    defaultStoreBase
                            &= typFile
  , policy_store          =    (defaultAssets ++ "ProjMan/" ++ defaultPolicies)
                            &= typFile
  , log_store_basename    =    defaultLogBase
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

defaultConf =
     modes [gclass, projman, hotcrp]
  &= summary "Generate Cedar case studies"
  &= program "cedar-restrict-case-stuy"

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
  conf
  { size_start = size_start'
  , size_incr  = size_incr'
  , size_end   = size_end'
  , priv_rep_ratio = priv_rep_ratio & max 0 & min 1
  , over_priv_percent = over_priv_percent & max 0 & min 50
  }
  where
    size_start' = max size_start 0
    size_incr'  = max size_incr 1
    size_end'   = max size_end size_start'
preprocess conf@PM{..} =
  conf
  { size_start = size_start'
  , size_incr  = size_incr'
  , size_end   = size_end'
  , priv_rep_ratio = priv_rep_ratio & max 0 & min 1
  , over_priv_percent = over_priv_percent & max 0 & min 50
  }
  where
    size_start' = max size_start 0
    size_incr'  = max size_incr 1
    size_end'   = max size_end size_start'
preprocess conf = conf
