{-# LANGUAGE ParallelListComp, RecordWildCards #-}
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
  cnf <- do
    tmp <- cmdArgs defaultConf
    return $ tmp & preprocess
  case cnf of
    GC{..} -> gc cnf
    PM{..} -> pm cnf
    HC{..} -> hc cnf
  where
    gc :: Config -> IO ()
    gc conf@GC{..} = do
      let gen = mkStdGen seed
      let (gclassFam, g') = GC.randomGClassroom conf & flip runState gen
      [ encodeFile path (gcls & GC.toGClassEntities)
        | gcls <- gclassFam & toPoolsGen GC.mergeGClassroom
        | path <- conf & entityStoreFP
        ] & sequence
      let (logFam, _) = GC.createEventLog conf gclassFam & flip runState g'
      -- sanity check: even though it will take longer, rerun old requests under
      -- a new entity store to help detect bugs
      putStrLn . show $ length logFam {- logFam & length -}
      resFam <-
        [ do
            ret <-
              forM log
                (\ req -> do
                    dec <- authorize' (CedarCtxt "cedar" Nothing entityStore policy_store) req
                    return $ LogEntry req dec)
            putStrLn . show $ i
            return ret
          | log <- logFam
          | entityStore <- conf & entityStoreFP
          | i <- conf & sizes
        ] & sequence
      [ do
          putStrLn path
          encodeFile path res
        | res <- toPools resFam
        | path <- conf & logStoreFP
        ] & sequence
      return ()

    pm :: Config -> IO ()
    pm conf@PM{..} = do
      let gen = mkStdGen seed
      let (projmanFam, g') = PM.randomProjMan conf & flip runState gen
      [ encodeFile path (pms & PM.toPMEntities)
        | pms <- projmanFam & toPoolsGen PM.mergeProjMan
        | path <- conf & entityStoreFP
        ] & sequence
      let (logFam, _) = PM.createEventLog conf projmanFam & flip runState g'
      resPool <-
        [ forM log
            (\ req -> do
                dec <- authorize' (CedarCtxt "cedar" Nothing entityStore policy_store) req
                return $ LogEntry req dec)
        | log <- logFam & toPools
        | entityStore <- conf & entityStoreFP
        ] & sequence
      [ encodeFile path res
        | res <- resPool
        | path <- conf & logStoreFP
        ] & sequence
      return ()
      -- PM.toPMEntities projman & encodeFile entityStore
      -- let (log, _) = PM.createEventLog conf projman & flip runState g'
      -- res <-
      --   forM log $ \req -> do
      --     dec <-
      --       authorize'
      --         (CedarCtxt "cedar" Nothing entityStore policyStore)
      --         req
      --     return $ LogEntry req dec
      -- encodeFile logs res

    hc :: Config -> IO ()
    hc conf@HC{..} = do
      let gen = mkStdGen seed
      let (hotcrp, g') = HC.randomHotCRP conf & flip runState gen
      HC.toHCEntities hotcrp & encodeFile entityStore
