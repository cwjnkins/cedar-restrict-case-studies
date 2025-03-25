{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}

module HotCRP.GenEntities where

import Control.Monad
import Control.Monad.State.Strict
import Data.Function
import System.Random

import Lib
import Lib.HotCRP
import Lib.Util

import Config

numAreasSafe :: Config -> Int
numAreasSafe HC{..} =
  max 1 numAreas

numPCSafe :: Config -> Int
numPCSafe HC{..} =
  foldl max 1 [numPC, numAreas, numPCChairs]

numNonPCSafe :: Config -> Int
numNonPCSafe HC{..} = max 1 numNonPC

maxAreaPapersSafe :: Config -> Int
maxAreaPapersSafe HC{..} = max 1 maxAreaPapers

maxPaperAuthorsSafe :: Config -> Int
maxPaperAuthorsSafe HC{..} = max 1 maxPaperAuthors

maxPaperReviewersSafe :: Config -> Int
maxPaperReviewersSafe HC{..} = max 1 maxPaperReviewers

generateAreas :: Int -> [Area]
generateAreas num = map (mkArea . show) [0..num-1]

-- MUST ensure PC, area chairs are always PC members:
--    numPCChairs <= numPCCommittee
-- && numAreaChairs <= numPCCommittee
--
-- PERMIT multiple PC chairs
-- PERMIT overlap between PC chairs and area chairs
--
-- ASSUME bijection: area, area chair

randomPC :: RandomGen g => Config -> [Area] -> State g [User]
-- REQUIRE length areas == numAreas
randomPC conf@HC{..} areas = do
  pc <- pcWAreaChairPCChair
  pc
    & map (\ (id, area, isAreaChair, isPCChair)
           -> mkUser (show id) isPCChair
                (if isAreaChair then Just area else Nothing)
                (Just area))
    & return
  where

  numberedPC :: [Int]
  numberedPC = [0..(numPCSafe conf - 2)]

  pcWArea :: [(Int, Area)]
  pcWArea = zip numberedPC (concat . repeat $ areas)

  pcWAreaChair :: [(Int, Area, Bool)]
  pcWAreaChair =
    let (chairs, others) = splitAt numAreas pcWArea
    in
       map (\ (id, area) -> (id, area, True)) chairs
    ++ map (\ (id, area) -> (id, area, False)) others

  pcWAreaChairPCChair :: RandomGen g => State g [(Int, Area, Bool, Bool)]
  pcWAreaChairPCChair = do
    shuff <- uniformShuffleList pcWAreaChair
    let (chairs, others) = splitAt numPCChairs shuff
    return $
         map (\ (id, area, areaChair) -> (id, area, areaChair, True)) chairs
      ++ map (\ (id, area, areaChair) -> (id, area, areaChair, False)) others

generateNonPC :: Config -> [User]
generateNonPC conf@HC{..} =
  let npc  = numPCSafe conf
      nnpc = numNonPCSafe conf
  in
    [npc..(npc+nnpc-1)]
  & map (\ id -> mkUser (show id) False Nothing Nothing)

randomUsers :: RandomGen g => Config -> [Area] -> State g [User]
randomUsers conf areas = do
  pc <- randomPC conf areas
  let others = generateNonPC conf
  return (pc ++ others)

randomPapers :: RandomGen g => Config -> [Area] -> [User] -> State g [Paper]
randomPapers conf areas users = do
  areaNumPaper <- randomNumPapers
  paperDataByArea :: [[([User], [User], Area)]] <- do
    forM areaNumPaper $ \ (area, numPapers) ->
      replicateM numPapers (randomPaperDataForArea area)
  let paperData = concat paperDataByArea
  return $ zipWith fromPaperData paperData [0..]
  where
  randomNumPapers :: RandomGen g => State g [(Area, Int)]
  randomNumPapers = forM areas $ \ area -> do
    numPapers <- state $ randomR (1, maxAreaPapersSafe conf)
    return (area, numPapers)

  randomPaperDataForArea :: RandomGen g => Area -> State g ([User], [User], Area)
  randomPaperDataForArea area = do
    (reviewers, nonReviewers) :: ([User], [User]) <- do
      numReviewers <- state $ randomR (1, maxPaperReviewersSafe conf)
      let (possibleReviewers, necessaryNonReviewers) = partitionUsersByPCArea area users
      shuff <- uniformShuffleList possibleReviewers
      let (reviewers, nonReviewersInPool) = splitAt numReviewers shuff
      return (reviewers, nonReviewersInPool ++ necessaryNonReviewers)

    authors <- do
      numAuthors <- state $ randomR (1, maxPaperAuthorsSafe conf)
      shuff <- uniformShuffleList nonReviewers
      return $ take numAuthors shuff

    return (authors, reviewers, area)

  fromPaperData :: ([User], [User], Area) -> Int -> Paper
  fromPaperData (authors, reviewers, area) id =
    mkPaper (show id) authors reviewers area

randomReviewsForPaper :: RandomGen g => Paper -> Int -> [User] -> State g [Review]
randomReviewsForPaper paper pid users = do
  let (reviewers, reviewerChairs) = reviewersForPaper paper users
  let normalReviews =
          reviewers
        & zipWith mkNormalReview [0..]
  metaReview <- do
    metaPool <- state $ randomR (False,True)
    -- it could be that the PC and area chairs are marked reviewers
    if metaPool || length reviewerChairs == 0 then do
      idx <- state $ randomR (0, length reviewers - 1)
      return $ mkMetaReview (reviewers !! idx)
    else do
      idx <- state $ randomR (0, length reviewerChairs - 1)
      return $ mkMetaReview (reviewerChairs !! idx)
  return $ metaReview:normalReviews

  where
    mkNormalReview :: Int -> User -> Review
    mkNormalReview id reviewer =
      mkReview ("P" ++ show pid ++ "R" ++ show id)
        paper reviewer False

    mkMetaReview :: User -> Review
    mkMetaReview reviewer =
      mkReview ("P" ++ show pid ++ "M")
        paper reviewer True

randomReviews :: RandomGen g => [Paper] -> [User] -> State g [Review]
randomReviews papers users = do
  groupedReviews <-
    forM (papers & zip [0..])
      (\ (id, paper) -> randomReviewsForPaper paper id users)
  return $ groupedReviews & concat

randomHotCRP :: RandomGen g => Config -> State g HotCRP
randomHotCRP conf@HC{..} = do
  let areas = generateAreas (numAreasSafe conf)
  users <- randomUsers conf areas
  papers <- randomPapers conf areas users
  reviews <- randomReviews papers users
  return $ HotCRP users areas papers reviews
