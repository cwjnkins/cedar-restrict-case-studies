{-# LANGUAGE RecordWildCards #-}
module HotCRP.GenLogs where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.HotCRP
import Lib.Util

import Config

randomAuthorForPaper :: RandomGen g => HotCRP -> Paper -> State g User
randomAuthorForPaper hc@HotCRP{..} paper = do
  let paperAuthors = paper & attrs & authors
  uid <- randomElem paperAuthors
  return $ hc & getUser uid

pAuthorActsOwnSubmission :: RandomGen g => HotCRPAction -> HotCRP -> Paper -> State g Request'
pAuthorActsOwnSubmission act hc paper = do
  author <- randomAuthorForPaper hc paper
  return $ act & toAction & toRequest' author paper
    -- mkRequest' author (toAction act) paper

pAuthorReadsOwnSubmission   :: RandomGen g => HotCRP -> Paper -> State g Request'
pAuthorUpdatesOwnSubmission :: RandomGen g => HotCRP -> Paper -> State g Request'
pAuthorDeletesOwnSubmission :: RandomGen g => HotCRP -> Paper -> State g Request'

pAuthorReadsOwnSubmission = pAuthorActsOwnSubmission Read
pAuthorUpdatesOwnSubmission = pAuthorActsOwnSubmission Update
pAuthorDeletesOwnSubmission = pAuthorActsOwnSubmission Delete

pAuthorReadsReview :: RandomGen g => HotCRP -> Review -> State g Request'
pAuthorReadsReview hc review = do
  let paper = hc & getPaper (review & attrs & ofPaper)
  author <- randomAuthorForPaper hc paper
  return $ Read & toAction & toRequest' author review
