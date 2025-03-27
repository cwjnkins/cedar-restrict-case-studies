{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module GClassroom.GenLogs where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.GClassroom
import Lib.Util

import Config

exercisedOverprivilegeSafe :: Config -> Int
exercisedOverprivilegeSafe GC{..} =
  exercisedOverprivilege & max 0 & min 100

pPostAssignment :: GClassroom -> Assignment -> Request'
pPostAssignment gc assign =
  let teacher = assign & getTeacher gc in
  postAssignment teacher assign

eopPostAssignment :: RandomGen g => GClassroom -> State g Request'
eopPostAssignment gc@GClassroom{..} = do
  ta <- randomElem tas
  course <- randomElem (ta & getCourses gc)
  assignment <- randomElem (course & getAssignments gc)
  return $ postAssignment ta assignment

pEditAssignment :: GClassroom -> Assignment -> Request'
pEditAssignment gc assign =
  let c = assign & getCourse gc
      t = c & getTeacher gc
  in
  editAssignment t assign

-- dEditAssignment :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- dEditAssignment gc assign = do
--   let c = getCourse gc assign
--   s <- randomElem (getStudents gc c)
--   return $ mkRequest' s (toAction EditAssignment) assign

eopEditAssignment :: RandomGen g => GClassroom -> State g Request'
eopEditAssignment gc@GClassroom{..} = do
  ta <- randomElem tas
  course <- randomElem (ta & getCourses gc) -- ta & getStaffCourses gc & randomElem
  assign <- randomElem (course & getAssignments gc)
  return $ editAssignment ta assign

-- All grades will have a permitted post request, but the principal (course
-- staff) are randomly selected
pPostGrades :: RandomGen g => GClassroom -> Assignment -> State g [Request']
pPostGrades gc assign = do
  let c = assign & getCourse gc
  let staff@(teach:tas) = c & getStaff gc
  let grades = assign & getGrades gc
  forM grades
    (\ gr -> do
       principal <- randomElem staff
       return $ postGrade principal gr)

-- -- For a given assignment, some students (for the course associated with the
-- -- assignment) try to post their own grades -- these are denied (by
-- -- default-deny)
-- dPostGrade :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- dPostGrade gc assign = do
--   let c = assign & getCourse gc
--   s <- randomElem (c & getStudents gc)
--   let g = findGrade gc assign s
--   return $ PostGrade & toAction & toRequest' s g

-- there are no over-privileges to exercise for posting grades


-- Viewing grades: after an assignment is released, a random student comes to
-- the office hours of a random course staff member, leading to the staff member
-- viewing the student's grade
pViewGrade__Staff :: RandomGen g => GClassroom -> Assignment -> State g Request'
pViewGrade__Staff gc assign = do
  let c = assign & getCourse gc
  stud <- randomElem (c & getStudents gc)
  staffMember <- randomElem (c & getStaff gc)
  let gr = findGrade gc assign stud
  return $ staffViewGrade staffMember gr

-- EOP: some staff request to view a grade for an assignment that is not for a
-- course they're teaching
eopViewGrade__Staff :: RandomGen g => GClassroom -> State g Request'
eopViewGrade__Staff gc@GClassroom{..} = do
  let eopPrincipals :: [(Staff, [Course])] =
        gc
        & getAllStaff
        & map (\ staf -> (staf, courses & filter (notInCourse staf)))
        & filter (\ (_, cs) -> length cs > 0)
  (eopPrincipal, cs) <- randomElem eopPrincipals
  c <- randomElem cs
  gr <- randomElem (c & getGrades gc)
  return $ staffViewGrade eopPrincipal gr
  where
    notInCourse :: Staff -> Course -> Bool
    notInCourse s c =
      (s & uid) `notElem` (c & getStaff gc & map uid)

-- dViewGrades__StudentAssignment
--   :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- dViewGrades__StudentAssignment gc assign = do
--   let c = getCourse gc assign
--   s <- randomElem (getStudents gc c)
--   return $ mkRequest' s (toAction ViewGrades) assign

pViewGrades__Student :: RandomGen g => GClassroom -> Assignment -> State g Request'
pViewGrades__Student gc@GClassroom{..} assign = do
  let studs = assign & getStudents gc
  stud <- randomElem studs
  let gr = findGrade gc assign stud
  return $ studentViewGrade stud gr

-- dViewGrades__StudentStudent
--   :: RandomGen g => GClassroom -> State g Request'
-- dViewGrades__StudentStudent GClassroom{..} = do
--   s1 <- randomElem students
--   s2 <- students & filter (\s -> uid s /= uid s1) & randomElem
--   return $ mkRequest' s1 (toAction ViewGrades) s2

createEventLog :: RandomGen g => Config -> GClassroom -> State g [Request']
createEventLog conf@GC{..} gc@GClassroom{..} = do
  okReqs :: [[[Request']]] <- do
    forM assignments
      (\ assign -> do
          let post = pPostAssignment gc assign
          edits <- editAssignReqs assign
          grades <- gradeAssignment assign
          staffViews <- staffViewGrades assign
          studViews <- studViewGrades assign
          return $ [post]:edits:grades:staffViews:[studViews])

  let okReqsByPolicy = okReqs & foldl1 (zipWith (++))
  if exercisedOverprivilegeSafe conf == 0
    then return $ okReqsByPolicy & concat
    else do
    let numEOPByPolicy =
          okReqsByPolicy
          & map (\reqs -> numExercisedOverPriv (length reqs) (exercisedOverprivilegeSafe conf))
    eops <-
      numEOPByPolicy
      & zipWith ($)
          [ flip replicateM eopPostAssign -- TA posts an assignment
          , flip replicateM eopEditAssign -- TA edits an assignment
          , const (return [])             -- no overprivileges
          , flip replicateM eopStaffVG    -- staff not-of-course views grade
          , const (return [])             -- no overprivileges
          ]
      & sequence
    return $
      okReqsByPolicy
      & zipWith (++) eops
      & concat

  where
    -- approx. 1 in 4 assignments need updating after posting (not counting EOPs)
    assignmentNeedsUpdatingDist :: [Int]
    assignmentNeedsUpdatingDist = [0,0,0,1]

    -- returns either empty or single-element list
    editAssignReqs :: RandomGen g => Assignment -> State g [Request']
    editAssignReqs assign = do
      num <- randomElem assignmentNeedsUpdatingDist
      return $ replicate num (pEditAssignment gc assign)

    -- for each assignment, grades for all students are posted
    gradeAssignment :: RandomGen g => Assignment -> State g [Request']
    gradeAssignment assign = pPostGrades gc assign

    -- a random number of visits during office hours, up to a third of the
    -- enrollment size (but possibly with repeating student visits)
    staffViewGrades :: RandomGen g => Assignment -> State g [Request']
    staffViewGrades assign = do
      let studs = assign & getStudents gc
      num <- state $ randomR (0, length studs `div` 3)
      replicateM num $
        pViewGrade__Staff gc assign

    -- similar to the above, but for students viewing their own grade
    studViewGrades :: RandomGen g => Assignment -> State g [Request']
    studViewGrades assign = do
      let studs = assign & getCourse gc & getStudents gc
      num <- state $ randomR (0, length studs `div` 3)
      replicateM num $
        pViewGrades__Student gc assign

    eopPostAssign :: RandomGen g => State g Request'
    eopPostAssign = eopPostAssignment gc

    eopEditAssign :: RandomGen g => State g Request'
    eopEditAssign = eopEditAssignment gc

    eopStaffVG :: RandomGen g => State g Request'
    eopStaffVG = eopViewGrade__Staff gc
