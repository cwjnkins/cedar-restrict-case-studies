{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.Aeson
import Data.Function
import System.Console.CmdArgs
import System.Random

import Lib
import Lib.Util
import Lib.IO

import Config
import LogGen

generateStudents :: Int -> [Student]
generateStudents num =
  map
    (mkStudent . show)
    [0..num-1]

generateCourses :: [[Student]] -> [Course]
generateCourses studGroups =
  zipWith
    (\ cNo classBody ->
       mkCourse (show cNo) classBody)
    [0..(length studGroups - 1)] studGroups

generateAssignments :: [(Int,Course)] -> [Assignment]
generateAssignments =
  concatMap genAssign
  where
    genAssign :: (Int,Course) -> [Assignment]
    genAssign (numAssign, forCourse) =
      [0..numAssign-1]
      & map (    show
             >>> (++ ("C" ++ getEntityName forCourse))
             >>> ((flip mkAssignment) forCourse))

generateStaff :: [[Course]] -> [[Course]] -> ([Staff], [Staff])
generateStaff teacherWorkload taWorkload =
  let teachLen = length teacherWorkload in
  let taLen    = length taWorkload in
  let teachIDs = map show [0..teachLen-1] in
  let taIDs    = map show [teachLen..teachLen+taLen-1] in
  ( zipWith mkTeacher teachIDs teacherWorkload
  , zipWith mkTA taIDs taWorkload )

randomGClassroom :: RandomGen g => GCConfig -> State g GClassroom
randomGClassroom GCConfig{..} = do
  let studs = generateStudents numStudents
  courseSizes <- replicateM numCourses randomCourseSize
  courses <- do
    courseLoad <- mapM (randomCourseBody studs) courseSizes
    return $ generateCourses courseLoad
  assignments <- randomAssignments courses
  (teachers, tas) <- do
    teacherWorkload <- randomTeacherWorkload courses
    taWorkload <- randomTAWorkload courses
    return $ generateStaff teacherWorkload taWorkload
  return $ GClassroom
    { students = studs
    , courses  = courses
    , assignments = assignments
    , tas = tas
    , teachers = teachers
    }
  where
    realMaxClassSize = min maxClassSize numStudents
    realTeacherNum = numAdditionalTeachers+1
    realTANum = min numTAs numCourses

    randomCourseSize :: RandomGen g => State g Int
    randomCourseSize = state $
      randomR (1, realMaxClassSize)

    randomCourseBody :: RandomGen g => [Student] -> Int -> State g [Student]
    randomCourseBody studs csize = do
      shuff <- uniformShuffleList studs
      return $ take csize shuff

    randomAssignments :: RandomGen g => [Course] -> State g [Assignment]
    randomAssignments cs =
          pure generateAssignments
      <*> flip mapM cs
            (\ course -> do
               numAssign <- state (randomR (1, maxCourseAssignments))
               return (numAssign, course))

    randomTeacherWorkload :: RandomGen g => [Course] -> State g [[Course]]
    randomTeacherWorkload = loop (replicate realTeacherNum [])
      where
      loop :: RandomGen g => [[Course]] -> [Course] -> State g [[Course]]
      loop accum [] = return accum
      loop accum (c:cs) = do
        teacherIdx <- state (randomR (0, realTeacherNum-1))
        let accum' = modifyAt accum teacherIdx (c :)
        loop accum' cs

    randomTAWorkload :: RandomGen g => [Course] -> State g [[Course]]
    randomTAWorkload cs = do
      shuff <- uniformShuffleList (map (:[]) cs)
      return $ take realTANum shuff

main :: IO ()
main = do
  gcconf <- cmdArgs config
  let gen = mkStdGen (seed gcconf)
  let (gclass, g') = randomGClassroom gcconf & (flip runState gen)
  toGClassEntities gclass & encodeFile (entityStore gcconf)
  let (log, _) = createEventLog gcconf gclass & flip runState g'
  res <-
    forM log $ \ req -> do
      dec <-
        authorize'
          (CedarCtxt "cedar" Nothing (entityStore gcconf) (policyStore gcconf))
          req
      return $ LogEntry req dec
  encodeFile (logs gcconf) res
