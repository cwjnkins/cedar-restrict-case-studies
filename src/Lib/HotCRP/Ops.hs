{-# LANGUAGE RecordWildCards #-}

module Lib.HotCRP.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.HotCRP.Types

partitionUsersByPCArea :: Area -> [User] -> ([User], [User])
partitionUsersByPCArea area =
  partition $ \ user ->
    case (user & attrs & pcMember) of
      Nothing -> False
      Just areaUID -> areaUID == (area & uid)

-- returns the designated reviewers of paper and PC chairs able to act on
-- reviews for paper
reviewersForPaper :: Paper -> [User] -> ([User], [User])
reviewersForPaper paper users = (reviewers', chairs)
  where
    reviewersPartition :: ([User], [User])
    reviewersPartition =
        users
      & partition
          (\ user ->
             (user & uid) `elem` (paper & attrs & reviewers))

    reviewers' = reviewersPartition & fst

    chairs :: [User]
    chairs =
        reviewersPartition & snd
      & filter
          (\ user ->
                (user & attrs & isPCChair)
             || (user & attrs & areaChair & maybe False (== (paper & attrs & area))))
      & filter
          (\ user ->
             not $ (user & uid) `elem` (paper & attrs & reviewers))

getUser :: UID -> HotCRP -> User
getUser u HotCRP{..} =
    users
  & find (\user -> u == (user & uid))
  & fromJust

getPaper :: UID -> HotCRP -> Paper
getPaper u HotCRP{..} =
    papers
  & find (\ paper -> u == (paper & uid))
  & fromJust
