{-# LANGUAGE RecordWildCards #-}

module GClassroom.GenEntities where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.Function
import Data.List
import Data.Maybe
import System.Random

import Lib
import Lib.GClassroom
import Lib.Util

import Config

maxCourseLoadSafe :: Config -> Int
maxCourseLoadSafe GC{..} =
  maxCourseLoad
  & max 0
  & min numCourses

generateCourses :: Int -> [Course]
generateCourses numCourses =
  [0..numCourses-1] & map (mkCourse . show)

generateStudents :: [[Course]] -> [Student]
generateStudents courseLoads =
  courseLoads
  & zipWith
      (\ studId courseLoad ->
         mkStudent (show studId) courseLoad)
      [0..(length courseLoads - 1)]

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

generateGradesForAssignment :: [Course] -> [Student] -> Assignment -> [Grade]
generateGradesForAssignment courses studs assign =
  let course = assign & findAssignmentCourse courses
      enrolled = course & findCourseStudents studs in
  enrolled
  & map (\ s -> mkGrade (fmtGId s) s assign)
  where
    fmtGId :: Student -> String
    fmtGId stud =
      "A"
      ++ (assign & uid & __entity & _id)
      ++ "_S"
      ++ (stud & uid & __entity & _id)

generateGrades :: [Assignment] -> [Course] -> [Student] -> [Grade]
generateGrades assigns cours studs =
  assigns
  & concatMap (generateGradesForAssignment cours studs)

generateStaff :: [[Course]] -> [[Course]] -> ([Staff], [Staff])
generateStaff teacherWorkload taWorkload =
  let teachLen = length teacherWorkload in
  let taLen    = length taWorkload in
  let teachIDs = map show [0..teachLen-1] in
  let taIDs    = map show [teachLen..teachLen+taLen-1] in
  ( zipWith mkTeacher teachIDs teacherWorkload
  , zipWith mkTA taIDs taWorkload )

randomGClassroom :: RandomGen g => Config -> State g GClassroom
randomGClassroom GC{..} = do
  let courses = generateCourses numCourses
  studs <- do
    courseLoad <- replicateM numStudents (randomCourseLoad courses)
    return $ generateStudents courseLoad
  assignments <- randomAssignments courses
  (teachers, tas) <- do
    teacherWorkload <- randomTeacherWorkload courses
    taWorkload <- randomTAWorkload courses
    return $ generateStaff teacherWorkload taWorkload
  let grades = generateGrades assignments courses studs
  return $ GClassroom
    { students = studs
    , courses  = courses
    , assignments = assignments
    , grades = grades
    , tas = tas
    , teachers = teachers
    }
  where
    realTeacherNum = numAdditionalTeachers+1
    realTANum = min numTAs numCourses

    -- course load for a single student
    randomCourseLoad :: RandomGen g => [Course] -> State g [Course]
    randomCourseLoad courses = do
      shuff <- uniformShuffleList courses
      load <- state $ randomR (1,maxCourseLoadSafe conf)
      return $ shuff & take load

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

