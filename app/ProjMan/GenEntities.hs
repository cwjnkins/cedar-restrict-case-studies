{-# LANGUAGE BangPatterns, ScopedTypeVariables, RecordWildCards #-}
module ProjMan.GenEntities where

import Data.Function

import Control.Monad.State.Strict
import System.Random

import Lib.ProjMan
import Lib.Util

import Config

generateProjectsWOManagers :: Int -> [Project]
generateProjectsWOManagers num =
  [mkProject ("P" ++ show i) dummy | i <- [0..num-1]]
  where
    dummy :: User
    dummy = mkUser "DUMMY" (mkRole "Dummy") []

generateUsersWOProjects :: Config -> ([User], [User], [User], [User])
generateUsersWOProjects PM{..} = (devs, plans, pms, accts)
  where
    helper :: Role -> Int -> Int -> [User]
    helper rol offset num = [mkUser (show $ i + offset) rol [] | i <- [0..num-1]]

    -- Didn't feel like being clever with this
    devs  = helper roleDeveloper 0 numDevs
    plans = helper rolePlanner numDevs numPlanners
    pms   = helper rolePM (numDevs + numPlanners) numPMs
    accts = helper roleAccountant (numDevs + numPlanners + numPMs) numAccts

associateUsersToProjects :: RandomGen g => Config -> ProjMan -> State g ProjMan
associateUsersToProjects PM{..} pm@ProjMan{..} = do
  loop pm projects
  where
    users :: [User]
    users = developers ++ planners ++ progmanagers ++ accountants

    randomProjectMembers :: RandomGen g => State g [User]
    randomProjectMembers = do
      shuff <- uniformShuffleList users
      num :: Int <- state (randomR (1,maxProjSize))
      return $ take num shuff

    loop :: RandomGen g => ProjMan -> [Project] -> State g ProjMan
    loop pm [] = return pm
    loop pm (p:projs) = do
      projmems <- randomProjectMembers
      lead <- randomElem projmems
      let pm' =
              projmems
            & foldr (\u acc -> assignToProject acc u p) pm
            & \ pm' -> assignToLeadProject pm' lead p
      loop pm' projs

randomProjMan :: RandomGen g => Config -> State g ProjMan
randomProjMan pm@PM{..} = do
  let projs = generateProjectsWOManagers numProjs
  let (devs, plans, pms, accts) = generateUsersWOProjects pm
  let init =
        ProjMan
          { developers   = devs
          , planners     = plans
          , progmanagers = pms
          , accountants  = accts
          , projects     = projs }
  pm <- associateUsersToProjects pm init
  return pm
