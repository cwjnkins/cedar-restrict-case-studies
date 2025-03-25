{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}
module ProjMan.GenLogs where

import Data.Function
import Data.List
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.ProjMan
import Lib.Util

import Config

assemblePrincipals :: ProjMan -> Project -> [[User]] -> [[User]]
assemblePrincipals pm p us =
    map (filterUsersByProject p) us
  & filter (\ r -> length r /= 0)
  & ([findProjectManager pm p] :)

pRandomPrincipal :: RandomGen g => ProjMan -> Project -> [[User]] -> State g User
pRandomPrincipal pm p pss = do
  role <- randomElem (assemblePrincipals pm p pss)
  randomElem role

pRandomRequest :: RandomGen g => ProjMan -> Project -> [[User]] -> PMAction -> State g Request'
pRandomRequest pm p rols act = do
  pr <- pRandomPrincipal pm p rols
  return $ mkRequest' pr (toAction act) p

-- N.B. In the case that a project only has one member (the project manager), we
-- will not be able to generate an exercised overprivilege
assembleEOPPrincipals :: ProjMan -> Project -> [[User]] -> [[User]]
assembleEOPPrincipals pm p us =
  let eopPrincipalsByRoles =
        us
        & map (filterUsersByProject p)
        & filter (\ r -> length r /= 0)
  in
  if null eopPrincipalsByRoles
  then [[findProjectManager pm p]]
  else eopPrincipalsByRoles

eopRandomPrincipal :: RandomGen g => ProjMan -> Project -> [[User]] -> State g User
eopRandomPrincipal pm p rols = do
  role <- randomElem (assembleEOPPrincipals pm p rols)
  randomElem role

eopRandomRequest :: RandomGen g => ProjMan -> Project -> [[User]] -> PMAction -> State g Request'
eopRandomRequest pm p rols act = do
  pr <- eopRandomPrincipal pm p rols
  return $ act & toAction & toRequest' pr p

pViewBudget :: RandomGen g => ProjMan -> Project -> State g Request'
pViewBudget pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers, accountants, planners] ViewBudget

eopViewBudget :: RandomGen g => ProjMan -> State g Request'
eopViewBudget pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [developers] ViewBudget

pEditBudget :: RandomGen g => ProjMan -> Project -> State g Request'
pEditBudget pm@ProjMan{..} p =
  pRandomRequest pm p [accountants, progmanagers] EditBudget

eopEditBudget :: RandomGen g => ProjMan -> State g Request'
eopEditBudget pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [developers, planners] EditBudget

pViewSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
pViewSchedule pm@ProjMan{..} p = do
  pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewSchedule

pEditSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
pEditSchedule pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers] EditSchedule

eopEditSchedule :: RandomGen g => ProjMan -> State g Request'
eopEditSchedule pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [developers, planners, accountants] EditSchedule

pViewAssets :: RandomGen g => ProjMan -> Project -> State g Request'
pViewAssets pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers, developers, planners] ViewAssets

eopViewAssets :: RandomGen g => ProjMan -> State g Request'
eopViewAssets pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [accountants] ViewAssets

pEditAssets :: RandomGen g => ProjMan -> Project -> State g Request'
pEditAssets pm@ProjMan{..} p =
  pRandomRequest pm p [developers] EditAssets

eopEditAssets :: RandomGen g => ProjMan -> State g Request'
eopEditAssets pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [planners, progmanagers, accountants] EditAssets

pViewCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
pViewCalendar pm@ProjMan{..} p =
  pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewCalendar

pEditCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
pEditCalendar pm@ProjMan{..} p =
  pRandomRequest pm p [planners] EditCalendar

eopEditCalendar :: RandomGen g => ProjMan -> State g Request'
eopEditCalendar pm@ProjMan{..} = do
  p <- randomElem projects
  eopRandomRequest pm p [developers, accountants, progmanagers] EditCalendar

createEventLog :: RandomGen g => Config -> ProjMan -> State g [Request']
createEventLog PM{..} pm@ProjMan{..} = do
  okReqs :: [(PMAction, Request')] <-
    replicateM (2 * maxCombinations `div` 3) $ do
      p <- randomElem projects
      a <- randomElem acts
      (a ,) <$>
        case a of
          ViewBudget -> pViewBudget pm p
          EditBudget -> pEditBudget pm p
          ViewSchedule -> pViewSchedule pm p
          EditSchedule -> pEditSchedule pm p
          ViewAssets -> pViewAssets pm p
          EditAssets -> pEditAssets pm p
          ViewCalendar -> pViewCalendar pm p
          EditCalendar -> pEditCalendar pm p
  let ret = okReqs & map snd
  if exercisedOverprivilege == 0
    then return ret
    else do
    let okReqsByPolicy :: [[(PMAction, Request')]] =
          okReqs
          & sortBy (\ (a1, _) (a2, _) -> compare a1 a2)
          & groupBy (\ (a1, _) (a2, _) -> a1 == a2)
    let numEOPsByPolicy :: [(PMAction, Int)] =
          okReqsByPolicy
          & map (\ reqs ->
                   ( reqs & head & fst
                   , numExercisedOverPriv (length reqs) exercisedOverprivilege))
    eopReqs <-
      forM numEOPsByPolicy (\ (act, num) -> do
        case act of
          ViewBudget -> eopViewBudget pm & replicateM num
          EditBudget -> eopEditBudget pm & replicateM num
          ViewSchedule -> return [] -- no overprivileges for view schedule
          EditSchedule -> eopEditSchedule pm & replicateM num
          ViewAssets -> eopViewAssets pm & replicateM num
          EditAssets -> eopEditAssets pm & replicateM num
          ViewCalendar -> return [] -- no overprivileges for view calendar
          EditCalendar -> eopEditCalendar pm & replicateM num)
    return $ ret ++ concat eopReqs
  where
    acts :: [PMAction]
    acts =
      [ ViewBudget, EditBudget
      , ViewSchedule, EditSchedule
      , ViewAssets, EditAssets
      , ViewCalendar, EditCalendar ]

    maxCombinations :: Int
    maxCombinations =
        length projects
      * length (pm & users)
      * length acts

