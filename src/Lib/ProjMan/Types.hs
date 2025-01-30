{-# LANGUAGE DeriveGeneric, RecordWildCards #-}

module Lib.ProjMan.Types where

import Data.Aeson
import Data.Function
import GHC.Generics

import Lib.Action
import Lib.Entity

type Role = Entity Value
mkRole :: String -> Role
mkRole name = Entity (mkUID "Role" name) (object []) []

roleDeveloper :: Role
roleDeveloper = mkRole "Developer"

rolePlanner :: Role
rolePlanner = mkRole "Planner"

rolePM :: Role
rolePM = mkRole "Program Manager"

roleAccountant :: Role
roleAccountant = mkRole "Accountant"

data UserAttrs = UserAttrs { role :: UID }
  deriving (Generic, Show)
instance ToJSON UserAttrs

type User = Entity UserAttrs

data ProjectAttrs = ProjectAttrs { manager :: UID }
  deriving (Generic, Show)
instance ToJSON ProjectAttrs

type Project = Entity ProjectAttrs
mkProject :: String -> User -> Project
mkProject name mngr =
  Entity
    (mkUID "Project" name)
    (ProjectAttrs (mngr & uid))
    []

mkUser :: String -> Role -> [Project] -> User
mkUser name rol projs =
  Entity
    (mkUID "User" name)
    (UserAttrs $ rol & uid)
    (map uid projs)

data ProjMan =
  ProjMan
  { developers   :: [User]
  , planners     :: [User]
  , progmanagers :: [User]
  , accountants  :: [User]
  , projects     :: [Project]
  }

users :: ProjMan -> [User]
users ProjMan{..} = developers ++ planners ++ progmanagers ++ accountants

data PMAction =
    ViewBudget
  | EditBudget
  | ViewSchedule
  | EditSchedule
  | ViewAssets
  | EditAssets
  | ViewCalendar
  | EditCalendar
  deriving Show

toAction :: PMAction -> Action
toAction act = Action . show $ act

data PMEntity =
    PMUser User
  | PMRole Role
  | PMProj Project
  deriving (Generic, Show)
instance ToJSON PMEntity where
  toJSON (PMUser u) = toJSON u
  toJSON (PMRole r) = toJSON r
  toJSON (PMProj p) = toJSON p

toPMEntities :: ProjMan -> [PMEntity]
toPMEntities ProjMan{..} =
     map PMRole [roleDeveloper, rolePlanner, rolePM, roleAccountant]
  ++ map PMUser developers
  ++ map PMUser planners
  ++ map PMUser progmanagers
  ++ map PMUser accountants
  ++ map PMProj projects
