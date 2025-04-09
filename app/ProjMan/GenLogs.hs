{-# LANGUAGE ParallelListComp, RecordWildCards, ScopedTypeVariables, TupleSections #-}
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
import ProjMan.Config

-- It is possible (though usually unlikely) that the over privilege for a rule
-- is empty; if so, default to sampling from the ideal privilege using project
-- managers (these request generators are only called if there's some new delta
-- of projects, meaning there must be some project manager in the delta)
assemblePrincipals :: ProjMan -> Bool -> [ProjMan -> [User]] -> [User]
assemblePrincipals pmDelta projectManagersP accessors =
  let managers = pmDelta & projectManagers
      nonmanagers =
        accessors
        & concatMap ($ pmDelta)
        & filter
            (\ u ->
               managers & not . any (\ u' -> (u' & uid) == (u & uid)))
  in
    if projectManagersP || null nonmanagers then
      managers ++ nonmanagers
    else
      nonmanagers

randomGenericRequest :: RandomGen g => ProjMan -> [User] -> PMAction -> State g Request'
randomGenericRequest pm principals act = do
  user    <- randomElem principals
  project <- randomElem (user & getUserProjects pm)
  return $ act & toAction & toRequest' user project

ipViewBudgetPrincipals pmDelta =
  assemblePrincipals pmDelta True
    [progmanagers,accountants,planners]

ipViewBudget :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipViewBudget pmDelta pm =
  randomGenericRequest pm (ipViewBudgetPrincipals pmDelta) ViewBudget

opViewBudgetPrincipals pmDelta =
  assemblePrincipals pmDelta False [developers]

opViewBudget :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opViewBudget pmDelta pm = do
  randomGenericRequest pm (opViewBudgetPrincipals pmDelta) ViewBudget

ipEditBudgetPrincipals pmDelta =
  assemblePrincipals pmDelta True [progmanagers,accountants]

ipEditBudget :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipEditBudget pmDelta pm = do
  randomGenericRequest pm (ipEditBudgetPrincipals pmDelta) EditBudget

opEditBudgetPrincipals pmDelta =
  assemblePrincipals pmDelta False [developers,planners]

opEditBudget :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opEditBudget pmDelta pm = do
  randomGenericRequest pm (opEditBudgetPrincipals pmDelta) EditBudget

-- no over privileges for ViewSchedule
ipViewSchedulePrincipals pmDelta = pmDelta & users

ipViewSchedule :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipViewSchedule pmDelta pm =
  randomGenericRequest pm (ipViewSchedulePrincipals pmDelta) ViewSchedule

ipEditSchedulePrincipals pmDelta =
  assemblePrincipals pmDelta True [progmanagers]

ipEditSchedule :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipEditSchedule pmDelta pm =
  randomGenericRequest pm (ipEditSchedulePrincipals pmDelta) EditSchedule

opEditSchedulePrincipals pmDelta =
  assemblePrincipals pmDelta False [planners, accountants, developers]

opEditSchedule :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opEditSchedule pmDelta pm =
  randomGenericRequest pm (opEditSchedulePrincipals pmDelta) EditSchedule

ipViewAssetsPrincipals pmDelta =
  assemblePrincipals pmDelta True [progmanagers, developers, planners]

ipViewAssets :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipViewAssets pmDelta pm =
  randomGenericRequest pm (ipViewAssetsPrincipals pmDelta) ViewAssets

opViewAssetsPrincipals pmDelta =
  assemblePrincipals pmDelta False [accountants]

opViewAssets :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opViewAssets pmDelta pm =
  randomGenericRequest pm (opViewAssetsPrincipals pmDelta) ViewAssets

ipEditAssetsPrincipals pmDelta =
  assemblePrincipals pmDelta True [developers]

ipEditAssets :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipEditAssets pmDelta pm =
  randomGenericRequest pm (ipEditAssetsPrincipals pmDelta) EditAssets

opEditAssetsPrincipals pmDelta =
  assemblePrincipals pmDelta False [progmanagers,accountants,planners]

opEditAssets :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opEditAssets pmDelta pm =
  randomGenericRequest pm (opEditAssetsPrincipals pmDelta) EditAssets

-- no over privilege for view calendar
ipViewCalendarPrincipals pmDelta =
  pmDelta & users

ipViewCalendar :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipViewCalendar pmDelta pm =
  randomGenericRequest pm (ipViewCalendarPrincipals pmDelta) ViewCalendar

ipEditCalendarPrincipals pmDelta =
  assemblePrincipals pmDelta True [planners]

ipEditCalendar :: RandomGen g => ProjMan -> ProjMan -> State g Request'
ipEditCalendar pmDelta pm =
  randomGenericRequest pm (ipEditCalendarPrincipals pmDelta) EditCalendar

opEditCalendarPrincipals pmDelta =
  assemblePrincipals pmDelta False [developers,progmanagers,accountants]

opEditCalendar :: RandomGen g => ProjMan -> ProjMan -> State g Request'
opEditCalendar pmDelta pm =
  randomGenericRequest pm (opEditCalendarPrincipals pmDelta) EditCalendar

createEventLog :: RandomGen g => Config -> Family ProjMan -> State g (Family [Request'])
createEventLog conf pmFam =
  [ do
      budgetViews <- randomViewBudgetReqs pmDelta (pm,pmOld)
      budgetEdits <- randomEditBudgetReqs pmDelta (pm,pmOld)
      scheduleViews <- randomViewScheduleReqs pmDelta (pm,pmOld)
      scheduleEdits <- randomEditScheduleReqs pmDelta (pm,pmOld)
      assetViews <- randomViewAssetReqs pmDelta (pm,pmOld)
      assetEdits <- randomEditAssetReqs pmDelta (pm,pmOld)
      calendarViews <- randomViewCalendarReqs pmDelta (pm,pmOld)
      calendarEdits <- randomEditCalendarReqs pmDelta (pm,pmOld)
      return $
           budgetViews   ++ budgetEdits
        ++ scheduleViews ++ scheduleEdits
        ++ assetViews    ++ assetEdits
        ++ calendarViews ++ calendarEdits
  | pmDelta <- pmFam
  | pm <- pmPool
  | pmOld <- emptyProjMan:(pmPool & init)
  ] & sequence
  where
    pmPool = pmFam & pmFamilyToPool

    -- sample number calculation
    repActual :: Int -> Int
    repActual cardActual =
      (cardActual & fromIntegral) * (conf & priv_rep_ratio) & ceiling

    repOver :: Int -> Int
    repOver cardActual =
      (repActual cardActual) * (conf & over_priv_percent) `div` 100

    repIdeal :: Int -> Int
    repIdeal cardActual =
      (repActual cardActual) - (repOver cardActual)

    calcNumSamples :: (Int,Int) -> (Int,Int)
    calcNumSamples (actualPriv, oldActualPriv) =
      ( (actualPriv & repIdeal) - (oldActualPriv & repIdeal)
      , (actualPriv & repOver)  - (oldActualPriv & repOver))

    -- cardinality
    cardinality :: ProjMan -> ([User],[User]) -> Int
    cardinality pm (ipPrincipals,opPrincipals) =
      let ic =
            [ (user,project)
            | user <- ipPrincipals
            , project <- user & getUserProjects pm
            ] & length
          oc =
            [ (user, project)
            | user <- opPrincipals
            , project <- user & getUserProjects pm
            ] & length
      in
        ic + oc

    cardViewBudgetActualPriv :: ProjMan -> Int
    cardViewBudgetActualPriv pm =
      cardinality pm (ipViewBudgetPrincipals pm, opViewBudgetPrincipals pm)

    cardEditBudgetActualPriv :: ProjMan -> Int
    cardEditBudgetActualPriv pm =
      cardinality pm (ipEditBudgetPrincipals pm, opEditBudgetPrincipals pm)

    cardViewScheduleActualPriv :: ProjMan -> Int
    cardViewScheduleActualPriv pm =
      cardinality pm (ipViewSchedulePrincipals pm, [])

    cardEditScheduleActualPriv :: ProjMan -> Int
    cardEditScheduleActualPriv pm =
      cardinality pm (ipEditSchedulePrincipals pm, opEditSchedulePrincipals pm)

    cardViewAssetsActualPriv :: ProjMan -> Int
    cardViewAssetsActualPriv pm =
      cardinality pm (ipViewAssetsPrincipals pm, opViewAssetsPrincipals pm)

    cardEditAssetsActualPriv :: ProjMan -> Int
    cardEditAssetsActualPriv pm =
      cardinality pm (ipEditAssetsPrincipals pm, opEditAssetsPrincipals pm)

    cardViewCalendarActualPriv :: ProjMan -> Int
    cardViewCalendarActualPriv pm =
      cardinality pm (ipViewCalendarPrincipals pm, [])

    cardEditCalendarActualPriv :: ProjMan -> Int
    cardEditCalendarActualPriv pm =
      cardinality pm (ipEditCalendarPrincipals pm, opEditCalendarPrincipals pm)

    -- requests
    randomViewBudgetReqs ::
      RandomGen g => ProjMan -> (ProjMan,ProjMan) -> State g [Request']
    randomViewBudgetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardViewBudgetActualPriv, pmOld & cardViewBudgetActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipViewBudget pmDelta pm
      overRep  <- replicateM overNum  $ opViewBudget pmDelta pm
      return $ idealRep ++ overRep

    randomEditBudgetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditBudgetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditBudgetActualPriv, pmOld & cardEditBudgetActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditBudget pmDelta pm
      overRep  <- replicateM overNum  $ opEditBudget pmDelta pm
      return $ idealRep ++ overRep

    randomViewScheduleReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewScheduleReqs pmDelta (pm, pmOld) = do
      let num =
            (pm & cardViewScheduleActualPriv, pmOld & cardViewScheduleActualPriv)
            & calcNumSamples
            & uncurry (+)
      replicateM num $ ipViewSchedule pmDelta pm

    randomEditScheduleReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditScheduleReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditScheduleActualPriv, pmOld & cardEditScheduleActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditSchedule pmDelta pm
      overRep  <- replicateM overNum  $ opEditSchedule pmDelta pm
      return $ idealRep ++ overRep

    randomViewAssetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewAssetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardViewAssetsActualPriv, pmOld & cardViewAssetsActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipViewAssets pmDelta pm
      overRep  <- replicateM overNum  $ opViewAssets pmDelta pm
      return $ idealRep ++ overRep

    randomEditAssetReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditAssetReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditAssetsActualPriv, pmOld & cardEditAssetsActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditAssets pmDelta pm
      overRep  <- replicateM overNum  $ opEditAssets pmDelta pm
      return $ idealRep ++ overRep

    randomViewCalendarReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomViewCalendarReqs pmDelta (pm, pmOld) = do
      let num =
            (pm & cardViewCalendarActualPriv, pmOld & cardViewCalendarActualPriv)
            & calcNumSamples
            & uncurry (+)
      replicateM num $ ipViewCalendar pmDelta pm

    randomEditCalendarReqs ::
      RandomGen g => ProjMan -> (ProjMan, ProjMan) -> State g [Request']
    randomEditCalendarReqs pmDelta (pm, pmOld) = do
      let (idealNum, overNum) =
            (pm & cardEditCalendarActualPriv, pmOld & cardEditCalendarActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipEditCalendar pmDelta pm
      overRep  <- replicateM overNum  $ opEditCalendar pmDelta pm
      return $ idealRep ++ overRep


  -- let byRole = assemblePrincipals pm [progmanagers,accountants,planners]
      --     byManager = assemblePrincipals pm [projectManagers]
      --     idealPrincipals =
      --       byRole
      --       ++ (byManager
      --           & filter
      --               (\ u ->
      --                  byRole
      --                  & not . any (\u' -> (u & uid) == (u' & uid))))
      --     overPrincipals =
      --     ic =
      --       [ (user, project)
      --       | user <- idealPrincipals
      --       , project <- user & getUserProjects pm
      --       ] & length
      --     oc =
      --       [ (user, project)
      --       | user <- assemblePrincipals pm [developers]
      --       , project <- user & getUserProjects pm
      --       ]
      -- in
      --   _ + _

-- pRandomPrincipal :: RandomGen g => ProjMan -> Project -> [[User]] -> State g User
-- pRandomPrincipal pm p pss = do
--   role <- randomElem (assemblePrincipals pm p pss)
--   randomElem role

-- pRandomRequest :: RandomGen g => ProjMan -> Project -> [[User]] -> PMAction -> State g Request'
-- pRandomRequest pm p rols act = do
--   pr <- pRandomPrincipal pm p rols
--   return $ mkRequest' pr (toAction act) p

-- -- N.B. In the case that a project only has one member (the project manager), we
-- -- will not be able to generate an exercised overprivilege
-- assembleEOPPrincipals :: ProjMan -> Project -> [[User]] -> [[User]]
-- assembleEOPPrincipals pm p us =
--   let eopPrincipalsByRoles =
--         us
--         & map (filterUsersByProject p)
--         & filter (\ r -> length r /= 0)
--   in
--   if null eopPrincipalsByRoles
--   then [[findProjectManager pm p]]
--   else eopPrincipalsByRoles

-- eopRandomPrincipal :: RandomGen g => ProjMan -> Project -> [[User]] -> State g User
-- eopRandomPrincipal pm p rols = do
--   role <- randomElem (assembleEOPPrincipals pm p rols)
--   randomElem role

-- eopRandomRequest :: RandomGen g => ProjMan -> Project -> [[User]] -> PMAction -> State g Request'
-- eopRandomRequest pm p rols act = do
--   pr <- eopRandomPrincipal pm p rols
--   return $ act & toAction & toRequest' pr p

-- pViewBudget :: RandomGen g => ProjMan -> Project -> State g Request'
-- pViewBudget pm@ProjMan{..} p =
--   pRandomRequest pm p [progmanagers, accountants, planners] ViewBudget

-- eopViewBudget :: RandomGen g => ProjMan -> State g Request'
-- eopViewBudget pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [developers] ViewBudget

-- pEditBudget :: RandomGen g => ProjMan -> Project -> State g Request'
-- pEditBudget pm@ProjMan{..} p =
--   pRandomRequest pm p [accountants, progmanagers] EditBudget

-- eopEditBudget :: RandomGen g => ProjMan -> State g Request'
-- eopEditBudget pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [developers, planners] EditBudget

-- pViewSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
-- pViewSchedule pm@ProjMan{..} p = do
--   pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewSchedule

-- pEditSchedule :: RandomGen g => ProjMan -> Project -> State g Request'
-- pEditSchedule pm@ProjMan{..} p =
--   pRandomRequest pm p [progmanagers] EditSchedule

-- eopEditSchedule :: RandomGen g => ProjMan -> State g Request'
-- eopEditSchedule pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [developers, planners, accountants] EditSchedule

-- pViewAssets :: RandomGen g => ProjMan -> Project -> State g Request'
-- pViewAssets pm@ProjMan{..} p =
--   pRandomRequest pm p [progmanagers, developers, planners] ViewAssets

-- eopViewAssets :: RandomGen g => ProjMan -> State g Request'
-- eopViewAssets pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [accountants] ViewAssets

-- pEditAssets :: RandomGen g => ProjMan -> Project -> State g Request'
-- pEditAssets pm@ProjMan{..} p =
--   pRandomRequest pm p [developers] EditAssets

-- eopEditAssets :: RandomGen g => ProjMan -> State g Request'
-- eopEditAssets pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [planners, progmanagers, accountants] EditAssets

-- pViewCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
-- pViewCalendar pm@ProjMan{..} p =
--   pRandomRequest pm p [developers, planners, progmanagers, accountants] ViewCalendar

-- pEditCalendar :: RandomGen g => ProjMan -> Project -> State g Request'
-- pEditCalendar pm@ProjMan{..} p =
--   pRandomRequest pm p [planners] EditCalendar

-- eopEditCalendar :: RandomGen g => ProjMan -> State g Request'
-- eopEditCalendar pm@ProjMan{..} = do
--   p <- randomElem projects
--   eopRandomRequest pm p [developers, accountants, progmanagers] EditCalendar

-- createEventLog :: RandomGen g => Config -> ProjMan -> State g [Request']
-- createEventLog PM{..} pm@ProjMan{..} = do
--   okReqs :: [(PMAction, Request')] <-
--     replicateM (2 * maxCombinations `div` 3) $ do
--       p <- randomElem projects
--       a <- randomElem acts
--       (a ,) <$>
--         case a of
--           ViewBudget -> pViewBudget pm p
--           EditBudget -> pEditBudget pm p
--           ViewSchedule -> pViewSchedule pm p
--           EditSchedule -> pEditSchedule pm p
--           ViewAssets -> pViewAssets pm p
--           EditAssets -> pEditAssets pm p
--           ViewCalendar -> pViewCalendar pm p
--           EditCalendar -> pEditCalendar pm p
--   let ret = okReqs & map snd
--   if exercisedOverprivilege == 0
--     then return ret
--     else do
--     let okReqsByPolicy :: [[(PMAction, Request')]] =
--           okReqs
--           & sortBy (\ (a1, _) (a2, _) -> compare a1 a2)
--           & groupBy (\ (a1, _) (a2, _) -> a1 == a2)
--     let numEOPsByPolicy :: [(PMAction, Int)] =
--           okReqsByPolicy
--           & map (\ reqs ->
--                    ( reqs & head & fst
--                    , numExercisedOverPriv (length reqs) exercisedOverprivilege))
--     eopReqs <-
--       forM numEOPsByPolicy (\ (act, num) -> do
--         case act of
--           ViewBudget -> eopViewBudget pm & replicateM num
--           EditBudget -> eopEditBudget pm & replicateM num
--           ViewSchedule -> return [] -- no overprivileges for view schedule
--           EditSchedule -> eopEditSchedule pm & replicateM num
--           ViewAssets -> eopViewAssets pm & replicateM num
--           EditAssets -> eopEditAssets pm & replicateM num
--           ViewCalendar -> return [] -- no overprivileges for view calendar
--           EditCalendar -> eopEditCalendar pm & replicateM num)
--     return $ ret ++ concat eopReqs
--   where
--     acts :: [PMAction]
--     acts =
--       [ ViewBudget, EditBudget
--       , ViewSchedule, EditSchedule
--       , ViewAssets, EditAssets
--       , ViewCalendar, EditCalendar ]

--     maxCombinations :: Int
--     maxCombinations =
--         length projects
--       * length (pm & users)
--       * length acts

