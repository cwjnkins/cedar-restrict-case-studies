{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections #-}

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
import GClassroom.Config

-- Entity generation
--------------------------------------------------

-- aliases for readability
type PreStudent = Int
type PreTeacher = Int
type PreTA      = Int

type CourseCatalog = [Course] -- all courses
type CourseLoad    = [Course] -- courses of a student/teacher/ta

generatePreStudents :: Config -> Family [PreStudent]
generatePreStudents conf =
  [ [0..f-1]
  | f <- conf & numStudents ]

generatePreTeachers :: Config -> Family [PreTeacher]
generatePreTeachers conf =
  [ [0..f-1]
  | f <- conf & numTeachers ]

generatePreTAs :: Config -> Family [PreTA]
generatePreTAs conf =
  [ [0..f-1]
  | f <- conf & numTAs ]

-- Generate the course catalog together with teachers
randomCourseCatalogWithStaff
  :: RandomGen g => Config -> Family [PreTeacher]
     -> Family (State g (CourseCatalog,[Staff]))
randomCourseCatalogWithStaff conf teachersFam =
  [ catalog teachers
  | teachers <- teachersFam ]
  where
    courseload :: RandomGen g => State g Int
    courseload = state $ randomR (1, conf & maxTeacherCourseload)

    create :: (PreTeacher, Int) -> (Staff,CourseLoad)
    create (teach,numCourses) =
      let teacherCourses :: CourseLoad =
            [ mkCourse ("T" ++ show teach ++ "C" ++ show cno)
            | cno <- [0..numCourses-1] ]
      in
      (mkTeacher (show teach) teacherCourses , teacherCourses)

    catalog :: RandomGen g => [PreTeacher] -> State g (CourseCatalog,[Staff])
    catalog teachers = do
      assocTeachCourseload :: [(Staff, CourseLoad)] <-
        forM teachers (\ t -> (t,) <$> courseload)
        & fmap (map create)
      return $
        ( assocTeachCourseload & concatMap snd
        , assocTeachCourseload & map fst)

-- Randomly generate the courseload for a student.
--
-- The courses comprising the courseload are picked randomly (without
-- replacement) from the `courses` parameter
randomStudentCourseload
  :: RandomGen g => Family CourseCatalog -> Family (State g)

-- randomStudentCourseload :: RandomGen g => [Course] -> State g [Course]
-- randomStudentCourseload courses = do
--   shuff <- uniformShuffleList courses
--   load <- state $ randomR (1, maxCourseLoadSafe conf)
--   return $ shuff & take load

-- Create students from their courseloads
--
-- ENSURES length courseLoads == length (generateStudents courseLoads)
generateStudents :: [[Course]] -> [Student]
generateStudents courseLoads =
  courseLoads
  & zipWith
      (\ studId courseLoad ->
         mkStudent (show studId) courseLoad)
      [0..(length courseLoads - 1)]

-- `randomStudents numStuds courses` randomgly generates `numStuds` students
-- with courseloads pulled randomly (without replacement) from `courses`
randomStudents :: RandomGen g => Int -> [Course] -> State g [Student]
randomStudents numStuds courses =
  generateStudents <$> replicateM numStuds (randomStudentCourseload courses)

-- Randomly generate courseloads for ever teacher.
--
-- NOTE: The invariant is that each course has precisely one teacher; a teacher
-- may have any number of courses (including 0)
randomTeacherCourseloads :: RandomGen g => Int -> [Course] -> State g [[Course]]
randomTeacherCourseloads numTeachers courses = do
  -- NOTE: A teacher might not have any courses
  assocTeachCourse :: [(Int,Course)] <-
    forM courses (\ c -> (, c) <$> randomTeacherId)
  return [courseload assocTeachCourse i | i <- [1..numTeachers-1]]
  where
    randomTeacherId :: RandomGen g => State g Int
    randomTeacherId = state $ randomR (0, numTeachers - 1)

    -- Given the teacher courseload (as pairs of teacher ids with a course),
    -- find all courses for the given teacher id
    courseload :: [(Int, Course)] -> Int -> [Course]
    courseload assoc idx =
      assoc
      & filter (\ (i,_) -> i == idx)
      & map snd

-- Randomly assign one course to one TA.
--
-- NOTE: The invariant is every TA has exactly one course, courses can have any
-- number of TAs (including 0)
randomTACourseloads :: RandomGen g => Int -> [Course] -> State g [Course]
randomTACourseloads numTAs courses =
  replicateM numTAs (randomElem courses)

-- Generate all staff, given the teacher and ta workloads.
--
-- Staff are assigned ids sequentially, with TAs numbered following teachers
generateStaff :: [[Course]] -> [[Course]] -> ([Staff], [Staff])
generateStaff teacherWorkload taWorkload =
  let teachLen = length teacherWorkload
      taLen    = length taWorkload
      teachIDs = ["TE" ++ show i | i <- [0..teachLen-1]]
      taIDs    = ["TA" ++ show i | i <- [0..taLen-1]]
  in
  ( zipWith mkTeacher teachIDs teacherWorkload
  , zipWith mkTA taIDs taWorkload )

randomStaff :: RandomGen g => Int -> Int -> [Course] -> State g ([Staff], [Staff])
randomStaff numTeach numTA courses = do
  teachCLs <- randomTeacherCourseloads numTeach courses
  taCLs    <- randomTACourseloads numTA courses
  return $ generateStaff teachCLs (taCLs & map (:[]))

-- Generate all assignments using a list of all courses, each
-- tupled with the number of assignments for the course
--
-- UID Example: the first assignment for the second course has id "C1A0"
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


-- Randomly generate the number of assignments for a course
randomNumAssignments :: RandomGen g => Config -> State g Int
randomNumAssignments conf =
  state $ randomR (1, conf & maxCourseAssignmentsSafe)

-- Randomly generate assignments for each course, where the number of
-- assignments generate is itself random.
randomAssignments :: RandomGen g => Config -> [Course] -> State g [Assignment]
randomAssignments conf courses = do
  assocNumAssignsCourse :: [(Int, Course)] <-
    forM courses
      (\ c ->
         (,c) <$> randomNumAssignments conf)
  return $ generateAssignments assocNumAssignsCourse

-- Deterministically generate the grade entities for a given assignment.
--
-- The number of grades generated is the number of students enrolled in course
-- to which the assignment belongs. Note, the invariant we keep is that a grade
-- is uniquely determined by its fields (there are no two distinct grade
-- entities for the same (student, assignment) pair)
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

-- Randomly generate a classroom
randomGClassroom :: RandomGen g => Config -> State g GClassroom
randomGClassroom conf@GC{..} = do
  let courses = generateCourses numCourses
  studs <- randomStudents numStudents courses
  assignments <- randomAssignments conf courses
  (teachers, tas) <-
    randomStaff (conf & numTeachersSafe) (conf & numTAsSafe) courses
  let grades = generateGrades assignments courses studs
  return $ GClassroom
    { students = studs
    , courses  = courses
    , assignments = assignments
    , grades = grades
    , tas = tas
    , teachers = teachers
    }
