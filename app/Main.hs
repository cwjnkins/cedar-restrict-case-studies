{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}


module Main (main) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Aeson
import Data.Function
import System.Console.CmdArgs
import System.Random

import qualified Lib.GClassroom as GC
import qualified Lib.ProjMan    as PM
import qualified Lib.HotCRP     as HC

import Lib.IO

import Config
import qualified GClassroom.GenEntities as GC
import qualified GClassroom.GenLogs     as GC
import qualified ProjMan.GenEntities    as PM
import qualified ProjMan.GenLogs        as PM
import qualified HotCRP.GenEntities     as HC

main :: IO ()
main = do
  cnf <- cmdArgs conf
  case cnf of
    GC _ _ _ _ _ _ _ _ _ _ -> gc cnf
    PM _ _ _ _ _ _ _ _ _ _ -> pm cnf
    HC _ _ _ _ _ _ _ _ _ _ _ -> hc cnf
  where
    gc :: Config -> IO ()
    gc conf@GC{..} = do
      let gen = mkStdGen seed
      let (gclass, g') = GC.randomGClassroom conf & flip runState gen
      GC.toGClassEntities gclass & encodeFile entityStore
      let (log, _) = GC.createEventLog conf gclass & flip runState g'
      res <-
        forM log $ \ req -> do
          dec <-
            authorize'
              (CedarCtxt "cedar" Nothing entityStore policyStore)
             req
          return $ LogEntry req dec
      encodeFile logs res

    pm :: Config -> IO ()
    pm conf@PM{..} = do
      let gen = mkStdGen seed
      let (projman, g') = PM.randomProjMan conf & flip runState gen
      PM.toPMEntities projman & encodeFile entityStore
      let (log, _) = PM.createEventLog conf projman & flip runState g'
      res <-
        forM log $ \req -> do
          dec <-
            authorize'
              (CedarCtxt "cedar" Nothing entityStore policyStore)
              req
          return $ LogEntry req dec
      encodeFile logs res

    hc :: Config -> IO ()
    hc conf@HC{..} = do
      let gen = mkStdGen seed
      let (hotcrp, g') = HC.randomHotCRP conf & flip runState gen
      HC.toHCEntities hotcrp & encodeFile entityStore
