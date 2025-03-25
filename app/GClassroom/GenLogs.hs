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

eopPostNextAssignment :: RandomGen g => GClassroom -> State g Request'
eopPostNextAssignment gc@GClassroom{..} = do
  ta <- randomElem tas
  course <- ta & getStaffCourses gc & randomElem
  return $ PostNextAssignment & toAction & toRequest' ta course

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

eopEditAssignment :: RandomGen g => GClassroom -> State g Request'
eopEditAssignment gc@GClassroom{..} = do
  ta <- randomElem tas
  course <- ta & getStaffCourses gc & randomElem
  assign <- course & getCourseAssignments gc & randomElem
  return $ EditAssignment & toAction & toRequest' ta assign

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

eopViewGrades__StaffAssignment :: RandomGen g => GClassroom -> State g Request'
eopViewGrades__StaffAssignment gc@GClassroom{..} = do
  (staff, cs) <-
      gc
    & getAllStaff
    & map (\staff -> (staff, courses & filter (notStaffForCourse staff)))
    & filter (\ (staff, cs) -> length cs /= 0)
    & randomElem
  c <- randomElem cs
  assign <- c & getCourseAssignments gc & randomElem
  return $ ViewGrades & toAction & toRequest' staff assign
  where
    notStaffForCourse :: Staff -> Course -> Bool
    notStaffForCourse s c = (c & uid) `notElem` (s & parents)

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

createEventLog :: RandomGen g => Config -> GClassroom -> State g [Request']
createEventLog conf@GC{..} gc@GClassroom{..} = do
  okReqs :: [[[Request']]] <- do
    forM assignments (\ assign -> do
      posts <- postAssignReqs assign
      edits <- editAssignReqs assign
      grades <- gradeSubmissionReqs assign
      staffViews <- staffViewGradeReqs assign
      studViews <- studViewGradeReqs
      return $ posts:edits:grades:staffViews:[studViews])
  let okReqsByPolicy = okReqs & foldl1 (zipWith (++))
  let ret = concat okReqsByPolicy
  if exercisedOverprivilegeSafe conf == 0 then
    return ret
  else do
    let numEOPByPolicy =
          okReqsByPolicy
          & map (\ reqs -> numExercisedOverPriv (length reqs) exercisedOverprivilege)
    eops <-
        numEOPByPolicy
      & zipWith ($)
          [ flip replicateM eopPostAssign  -- TA posts an assignment
          , flip replicateM eopEditAssign  -- TA edits an assignment
          , const (return [])              -- no overprivileges
          , flip replicateM eopStaffVG     -- staff views grades of assignment for course they're not staff of
          , const (return [])              -- no overprivileges
          ]
      & sequence
    return $ ret ++ (eops & concat)
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

    staffViewGradeReqs :: RandomGen g => Assignment -> State g [Request']
    staffViewGradeReqs assign = do
      let pVGStaff = pViewGrades__StaffAssignment gc assign
      return pVGStaff

    studViewGradeReqs :: RandomGen g => State g [Request']
    studViewGradeReqs = do
      pVGStudent :: [Request'] <- do
        num <- randomElem [1..maxClassSize]
        replicateM num (pViewGrades__StudentStudent gc)
      dVGStudent :: [Request'] <- do
        num <- randomElem studentUnauthRequestDistPerCourse
        replicateM num (dViewGrades__StudentStudent gc)
      return $ pVGStudent ++ dVGStudent

    -- numExercisedOverPriv :: [Request'] -> Int
    -- numExercisedOverPriv reqs = (reqNum * proportionAdditional) & floor
    --   where
    --   reqNum = reqs & length & fromIntegral
    --   okPercent = ((100 - exercisedOverprivilegeSafe conf) & fromIntegral)
    --   proportionAdditional :: Double
    --   proportionAdditional = (100.0 / okPercent) - 1.0

    eopPostAssign :: RandomGen g => State g Request'
    eopPostAssign = eopPostNextAssignment gc

    eopEditAssign :: RandomGen g => State g Request'
    eopEditAssign = eopEditAssignment gc

    eopStaffVG :: RandomGen g => State g Request'
    eopStaffVG = eopViewGrades__StaffAssignment gc
