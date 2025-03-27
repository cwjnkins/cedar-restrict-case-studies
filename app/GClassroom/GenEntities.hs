{-# LANGUAGE RecordWildCards #-}

module GClassroom.GenEntities where

import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Data.Function
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

-- Deterministically create courses
generateCourses :: Int -> [Course]
generateCourses numCourses =
  [0..numCourses-1] & map (mkCourse . show)

-- Deterministically create students, given each of their courseloads
-- REQUIRES forall courseLoad `elem` courseLoads, nub courseLoad == courseLoad
-- ENSURES length courseLoads == length (generateStudents courseLoads)
generateStudents :: [[Course]] -> [Student]
generateStudents courseLoads =
  courseLoads
  & zipWith
      (\ studId courseLoad ->
         mkStudent (show studId) courseLoad)
      [0..(length courseLoads - 1)]

-- Deterministically all assignments using a list of all courses, each tupled
-- with the number of assignments for the course
-- Example: the first assignment for the second course has id "C1A0"
generateAssignments :: [(Int,Course)] -> [Assignment]
generateAssignments =
  concatMap genAssign
  where
    genAssign :: (Int,Course) -> [Assignment]
    genAssign (numAssign, forCourse) =
      [0..numAssign-1]
      & map
          (\ id ->
             let fullId =
                   "C" ++ (forCourse & getEntityName)
                   ++ "A" ++ show id
             in
             mkAssignment fullId forCourse)

-- Deterministically generate the grade entities for a given assignment.
--
-- The number of grades generated is the number of students enrolled in course
-- to which the assignment belongs. Note, the invariant we keep is that a grade
-- is uniquely determined by its fields (there are no two grade entities for the
-- same (student, assignment) pair)
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

-- Deterministically generate all grades for every pair of assignment and
-- student in the course for the assignment
generateGrades :: [Assignment] -> [Course] -> [Student] -> [Grade]
generateGrades assigns cours studs =
  assigns
  & concatMap (generateGradesForAssignment cours studs)


-- Deterministically generate all course staff, given the teacher and ta
-- workloads.
--
-- Staff are assigned ids sequentially, with TAs numbered following teachers
generateStaff :: [[Course]] -> [[Course]] -> ([Staff], [Staff])
generateStaff teacherWorkload taWorkload =
  let teachLen = length teacherWorkload in
  let taLen    = length taWorkload in
  let teachIDs = map show [0..teachLen-1] in
  let taIDs    = map show [teachLen..teachLen+taLen-1] in
  ( zipWith mkTeacher teachIDs teacherWorkload
  , zipWith mkTA taIDs taWorkload )

-- Randomly generate a classroom
randomGClassroom :: RandomGen g => Config -> State g GClassroom
randomGClassroom GC{..} = do
  let courses = generateCourses numCourses
  studs <- do
    -- randomly generate a course load `numStudents` times
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
    -- at most one TA for a course
    realTANum = min numTAs numCourses

    -- Randomly generate a single student course load.
    -- - `load` is in the range (1, maxCourseLoad)
    -- - the courses comprising the courseload are `load` distinct course
    randomCourseLoad :: RandomGen g => [Course] -> State g [Course]
    randomCourseLoad courses = do
      shuff <- uniformShuffleList courses
      load <- state $ randomR (1,maxCourseLoadSafe conf)
      return $ shuff & take load

    -- For each course, randomly generate some number of assignments (in the
    -- range (1,maxCourseAssignments))
    randomAssignments :: RandomGen g => [Course] -> State g [Assignment]
    randomAssignments cs =
          pure generateAssignments
      <*> forM cs
            (\ course -> do
               numAssign <- state (randomR (1, maxCourseAssignments))
               return (numAssign, course))

    -- Teachers can have more than one course, but courses have exactly one
    -- teacher. So, iterate through all courses and assign each randomly to a
    -- teacher (represented as a list of course loads)
    randomTeacherWorkload :: RandomGen g => [Course] -> State g [[Course]]
    randomTeacherWorkload = loop (replicate realTeacherNum [])
      where
      loop :: RandomGen g => [[Course]] -> [Course] -> State g [[Course]]
      loop accum [] = return accum
      loop accum (c:cs) = do
        teacherIdx <- state (randomR (0, realTeacherNum-1))
        let accum' = modifyAt accum teacherIdx (c :)
        loop accum' cs

    -- TAs have precisely one course, and each course has at most one TA. So,
    -- shuffle the full list of courses and take the `realTANum` prefix as the
    -- TA workloads
    randomTAWorkload :: RandomGen g => [Course] -> State g [[Course]]
    randomTAWorkload cs = do
      shuff <- uniformShuffleList (map (:[]) cs)
      return $ take realTANum shuff

