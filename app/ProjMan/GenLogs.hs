{-# LANGUAGE RecordWildCards #-}
module ProjMan.GenLogs where

import Data.Function
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

pViewBudget :: RandomGen g => ProjMan -> Project -> State g Request'
pViewBudget pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers, accountants, planners] ViewBudget

pEditBudget :: RandomGen g => ProjMan -> Project -> State g Request'
pEditBudget pm@ProjMan{..} p =
  pRandomRequest pm p [accountants, progmanagers] EditBudget

pViewSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
pViewSchedule pm@ProjMan{..} p = do
  pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewSchedule

pEditSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
pEditSchedule pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers] EditSchedule

pViewAssets :: RandomGen g => ProjMan -> Project -> State g Request'
pViewAssets pm@ProjMan{..} p =
  pRandomRequest pm p [progmanagers, developers, planners] ViewAssets

pEditAssets :: RandomGen g => ProjMan -> Project -> State g Request'
pEditAssets pm@ProjMan{..} p =
  pRandomRequest pm p [developers] EditAssets

pViewCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
pViewCalendar pm@ProjMan{..} p =
  pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewCalendar

pEditCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
pEditCalendar pm@ProjMan{..} p =
  pRandomRequest pm p [planners] EditCalendar

createEventLog :: RandomGen g => Config -> ProjMan -> State g [Request']
createEventLog PM{..} pm@ProjMan{..} =
  replicateM (2 * maxCombinations `div` 3) $ do
    p <- randomElem projects
    a <- randomElem acts
    case a of
      ViewBudget -> pViewBudget pm p
      EditBudget -> pEditBudget pm p
      ViewSchedule -> pViewSchedule pm p
      EditSchedule -> pEditSchedule pm p
      ViewAssets -> pViewAssets pm p
      EditAssets -> pEditAssets pm p
      ViewCalendar -> pViewCalendar pm p
      EditCalendar -> pEditCalendar pm p
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

