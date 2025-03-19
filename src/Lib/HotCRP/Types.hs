{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Lib.HotCRP.Types where

import Data.Aeson
import GHC.Generics

import Lib.Action
import Lib.Entity

type Area = Entity Value
mkArea :: String -> Area
mkArea name = Entity (mkUID "Area" name) (object []) []

data UserAttrs
  = UserAttrs
    { isPCChair :: Bool
    , areaChair :: Maybe UID
    , pcMember :: Maybe UID}
  deriving (Generic, Show)

instance ToJSON UserAttrs where

type User = Entity UserAttrs
mkUser :: String -> Bool -> Maybe Area -> Maybe Area -> User
mkUser name isPCChair areaChair pcMember =
  let areaChair' = uid <$> areaChair
      pcMember'  = uid <$> pcMember
  in
  Entity (mkUID "User" name) (UserAttrs isPCChair areaChair' pcMember') []

data PaperAttrs
  = PaperAttrs
    { authors    :: [UID]
    , revieweres :: [UID]
    , area       :: UID
    }
  deriving (Generic, Show)

instance ToJSON PaperAttrs where

type Paper = Entity PaperAttrs
mkPaper :: String -> [User] -> [User] -> Area -> Paper
mkPaper name authors reviewers area =
  let authors'   = uid <$> authors
      reviewers' = uid <$> reviewers
      area'      = uid area
  in
  Entity
    (mkUID "Paper" name)
    (PaperAttrs authors' reviewers' area')
    []

data ReviewAttrs =
  ReviewAttrs
  { ofPaper :: UID
  , author  :: UID
  , isMetaReview :: Bool
  }
  deriving (Generic, Show)

type Review = Entity ReviewAttrs
mkReview :: String -> Paper -> User -> Bool -> Review
mkReview name ofPaper author isMetaReview =
  let ofPaper' = uid ofPaper
      author'  = uid author
  in
  Entity
    (mkUID "Review" name)
    (ReviewAttrs ofPaper' author' isMetaReview)
    []

data HotCRP = HotCRP
  { users   :: [User]
  , areas   :: [Area]
  , papers  :: [Paper]
  , reviews :: [Review]
  }

data HotCRPAction =
  ReleaseReview
  | Read
  | Update
  | Delete
  deriving (Show)

toAction :: HotCRPAction -> Action
toAction = Action . show

data HotCRPEntity =
  HotCRPUser User
  | HotCRPArea Area
  | HotCRPPaper Paper
  | HotCRPReview Review

toHotCRPEntities :: HotCRP -> [HotCRPEntity]
toHotCRPEntities (HotCRP {..}) =
     map HotCRPUser users
  ++ map HotCRPArea areas
  ++ map HotCRPPaper papers
  ++ map HotCRPReview reviews
