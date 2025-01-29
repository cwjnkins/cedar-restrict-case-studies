{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module LogGen where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.Util

import Config

pPostNextAssignment :: GClassroom -> Assignment -> Request'
pPostNextAssignment gc assign =
  let course = getCourse gc assign in
  let teacher = getTeacher gc course in
  mkRequest' teacher (toAction PostNextAssignment) course

dPostNextAssignment :: RandomGen g => GClassroom -> Assignment -> State g Request'
dPostNextAssignment gc assign = do
  let c = getCourse gc assign
  s <- randomElem (getStudents gc c)
  return $ mkRequest' s (toAction PostNextAssignment) c

pEditAssignment :: GClassroom -> Assignment -> Request'
pEditAssignment gc assign =
  let c = getCourse gc assign
      t = getTeacher gc c
  in
  mkRequest' t (toAction EditAssignment) assign

dEditAssignment :: RandomGen g => GClassroom -> Assignment -> State g Request'
dEditAssignment gc assign = do
  let c = getCourse gc assign
  s <- randomElem (getStudents gc c)
  return $ mkRequest' s (toAction EditAssignment) assign

pGradeSubmission :: RandomGen g => GClassroom -> Assignment -> State g [Request']
pGradeSubmission gc assign = do
  let c = getCourse gc assign
  let (t, assists) = getCourseStaff gc c
  let r1 = mkRequest' t (toAction GradeSubmission) assign
  tasGrading <- do
    numGrading <- state $ randomR (0, length assists - 1)
    take numGrading <$> uniformShuffleList assists
  let rs =
          tasGrading
        & map (\ ta -> mkRequest' ta (toAction GradeSubmission) assign)
  return (r1 : rs)

dGradeSubmission :: RandomGen g => GClassroom -> Assignment -> State g Request'
dGradeSubmission gc assign = do
  let c = getCourse gc assign
  s <- randomElem (getStudents gc c)
  return $ mkRequest' s (toAction GradeSubmission) assign

pViewGrades__StaffAssignment
  :: GClassroom -> Assignment -> [Request']
pViewGrades__StaffAssignment gc assign =
  let
    c = getCourse gc assign
    (t, assists) = getCourseStaff gc c
  in
  (  (t : assists)
   & map (\ p -> mkRequest' p (toAction ViewGrades) assign))

dViewGrades__StudentAssignment
  :: RandomGen g => GClassroom -> Assignment -> State g Request'
dViewGrades__StudentAssignment gc assign = do
  let c = getCourse gc assign
  s <- randomElem (getStudents gc c)
  return $ mkRequest' s (toAction ViewGrades) assign

pViewGrades__StudentStudent
  :: RandomGen g => GClassroom -> State g Request'
pViewGrades__StudentStudent GClassroom{..} = do
  s <- randomElem students
  return $ mkRequest' s (toAction ViewGrades) s

dViewGrades__StudentStudent
  :: RandomGen g => GClassroom -> State g Request'
dViewGrades__StudentStudent GClassroom{..} = do
  s1 <- randomElem students
  s2 <- students & filter (\s -> uid s /= uid s1) & randomElem
  return $ mkRequest' s1 (toAction ViewGrades) s2

createEventLog :: RandomGen g => GCConfig -> GClassroom -> State g [Request']
createEventLog GCConfig{..} gc@GClassroom{..} =
    forM assignments (\ assign -> do
      posts <- postAssignReqs assign
      edits <- editAssignReqs assign
      grades <- gradeSubmissionReqs assign
      views <- viewGradeReqs assign
      return $ posts ++ edits ++ grades ++ views)
  & (concat <$>)
  where
    studentUnauthRequestDistPerCourse =
        [1..maxClassSize]
      & map (`div` 3)
      & (1:)

    assignmentNeedsUpdatingDist :: [Int]
    assignmentNeedsUpdatingDist = [0,0,0,1]

    postAssignReqs :: RandomGen g => Assignment -> State g [Request']
    postAssignReqs assign = do
      studAttempts :: [Request'] <- do
        num <- randomElem studentUnauthRequestDistPerCourse
        replicateM num (dPostNextAssignment gc assign)
      return $ pPostNextAssignment gc assign : studAttempts

    editAssignReqs :: RandomGen g => Assignment -> State g [Request']
    editAssignReqs assign = do
      studAttempts :: [Request'] <- do
        num <- randomElem studentUnauthRequestDistPerCourse
        replicateM num (dEditAssignment gc assign)
      teachEdit :: [Request'] <- do
        num <- randomElem assignmentNeedsUpdatingDist
        return . replicate num $ pEditAssignment gc assign
      uniformShuffleList $ teachEdit ++ studAttempts

    gradeSubmissionReqs :: RandomGen g => Assignment -> State g [Request']
    gradeSubmissionReqs assign = do
      studAttempts :: [Request'] <- do
        num <- randomElem studentUnauthRequestDistPerCourse
        replicateM num (dGradeSubmission gc assign)
      staffReqs <- pGradeSubmission gc assign
      uniformShuffleList (staffReqs ++ studAttempts)

    viewGradeReqs :: RandomGen g => Assignment -> State g [Request']
    viewGradeReqs assign = do
      -- some students check their grades
      pVGStudent :: [Request'] <- do
        num <- randomElem [1..maxClassSize]
        replicateM num (pViewGrades__StudentStudent gc)
      -- staff always checks grades
      let pVGStaff = pViewGrades__StaffAssignment gc assign
      -- some students try to see other student grades
      dVGStudent :: [Request'] <- do
        num <- randomElem studentUnauthRequestDistPerCourse
        replicateM num (dViewGrades__StudentStudent gc)
      uniformShuffleList $ pVGStudent ++ pVGStaff ++ dVGStudent

