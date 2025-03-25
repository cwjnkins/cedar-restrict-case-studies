{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Lib.HotCRP.Types where

import Data.Aeson
import Data.Function
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
  toJSON UserAttrs{..} = object $
       ["isPCChair" .= isPCChair]
    ++ (areaChair & maybe [] (\ area -> ["areaChair" .= area]))
    ++ (pcMember  & maybe [] (\ area -> ["pcMember"  .= area]))

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
    , reviewers :: [UID]
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
instance ToJSON ReviewAttrs where

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
  deriving (Generic, Show)
instance ToJSON HotCRPEntity where
  toJSON (HotCRPUser user) = toJSON user
  toJSON (HotCRPArea area) = toJSON area
  toJSON (HotCRPPaper pap) = toJSON pap
  toJSON (HotCRPReview rv) = toJSON rv


toHCEntities :: HotCRP -> [HotCRPEntity]
toHCEntities (HotCRP {..}) =
     map HotCRPUser users
  ++ map HotCRPArea areas
  ++ map HotCRPPaper papers
  ++ map HotCRPReview reviews
