{-# LANGUAGE ParallelListComp, ScopedTypeVariables, RecordWildCards #-}
module GClassroom.GenLogs where

import Data.Function
import Control.Monad
import Control.Monad.State.Strict
import System.Random

import Lib
import Lib.GClassroom
import Lib.Util

import Config
import GClassroom.Config

pPostAssignment :: RandomGen g => GClassroom -> [Staff] -> State g Request'
pPostAssignment gc staffDelta = do
  staffMem <- randomElem staffDelta
  assignment <- randomElem (staffMem & getAssignments gc)
  return $ postAssignment staffMem assignment

pEditAssignment :: RandomGen g => GClassroom -> [Staff] -> State g Request'
pEditAssignment gc staffDelta = do
  staffMem <- randomElem staffDelta
  assignment <- randomElem (staffMem & getAssignments gc)
  return $ editAssignment staffMem assignment

pPostGrade :: RandomGen g => GClassroom -> [Staff] -> State g Request'
pPostGrade gc staffDelta = do
  staffMem <- randomElem staffDelta
  grade <- randomElem (staffMem & getGrades gc)
  return $ postGrade staffMem grade

ipViewGrade__Staff :: RandomGen g => GClassroom -> [Staff] -> State g Request'
ipViewGrade__Staff gc staffDelta = do
  staffMem <- randomElem staffDelta
  grade <- randomElem (staffMem & getGrades gc)
  return $ staffViewGrade staffMem grade

-- NOTE: There is no guarantee that a particular staff member has grades not
-- associated with them. Entity generation only guarantees there is at least one
-- staff member with this property
opViewGrade__Staff :: RandomGen g => GClassroom -> [Staff] -> State g Request'
opViewGrade__Staff gc staffDelta = do
  let prSet :: [(Staff,[Grade])] =
        staffDelta
        & map (\ s -> (s, s & getGradesNotForStaffMember gc))
        & filter (\ (s, grs) -> not (null grs))
  (staffMem, gradesNotForThem) <- randomElem prSet
  grade <- randomElem gradesNotForThem
  return $ staffViewGrade staffMem grade
  -- staffMem <- randomElem staffDelta
  -- grade <- randomElem (staffMem & getGradesNotForStaffMember gc)
  -- return $ staffViewGrade staffMem grade

pViewGrade__Student :: RandomGen g => GClassroom -> [Student] -> State g Request'
pViewGrade__Student gc studDelta = do
  stud <- randomElem studDelta
  grade <- randomElem (stud & getStudentGrades gc)
  return $ studentViewGrade stud grade

createEventLog ::
  RandomGen g => Config -> Family GClassroom -> State g (Family [Request'])
createEventLog conf gcFam =
  [ do
      postAssigns <- randomPostAssignmentReqs gcFamMem (gc, gcOld)
      editAssigns <- randomEditAssignmentReqs gcFamMem (gc, gcOld)
      postGrades  <- randomPostGradeReqs      gcFamMem (gc, gcOld)
      viewGradesStaff <- randomViewGradeReqs__Staff gcFamMem (gc, gcOld)
      viewGradesStuds <- randomViewGradeReqs__Student gcFamMem (gc, gcOld)
      return $
        postAssigns ++ editAssigns ++ postGrades ++ viewGradesStaff ++ viewGradesStuds
  | gcFamMem <- gcFam
  | gc <- gcPool
  | gcOld <- emptyGClassroom:(gcPool & init)
  ] & sequence
  where
    gcPool = gcFam & gcFamilyToPool

    -- sample number calculation
    repActual :: Int -> Int
    repActual cardActual =
      (cardActual & fromIntegral) * (conf & priv_rep_ratio) & ceiling

    repOver :: Int -> Int
    repOver cardActual =
      (repActual cardActual) * (conf & over_priv_percent) `div` 100

    repIdeal :: Int -> Int
    repIdeal cardActual =
      (repActual cardActual) - (repOver cardActual)

    calcNumSamples :: (Int,Int) -> (Int,Int)
    calcNumSamples (actualPriv, oldActualPriv) =
      ( (actualPriv & repIdeal) - (oldActualPriv & repIdeal)
      , (actualPriv & repOver)  - (oldActualPriv & repOver))

    -- cardinality
    cardPostAssignmentActualPriv :: GClassroom -> Int
    cardPostAssignmentActualPriv gc =
      -- ideal privilege cardinality
      let ic =
            [ (teacher, assignment)
            | teacher <- gc & teachers
            , assignment <- teacher & getAssignments gc
            ] & length
      -- over privilege cardinality
          oc =
            [ (ta, assignment)
            | ta <- gc & tas
            , assignment <- ta & getAssignments gc
            ] & length
      in
        ic + oc

    cardEditAssignmentActualPriv :: GClassroom -> Int
    cardEditAssignmentActualPriv = cardPostAssignmentActualPriv

    cardPostGradeActualPriv :: GClassroom -> Int
    cardPostGradeActualPriv gc =
      let ic =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGrades gc
            ] & length
          oc = 0
      in
        oc + ic

    cardViewGradeActualPriv__Staff :: GClassroom -> Int
    cardViewGradeActualPriv__Staff gc =
      let ic =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGrades gc
            ] & length
          oc =
            [ (staffMem, grade)
            | staffMem <- gc & getAllStaff
            , grade <- staffMem & getGradesNotForStaffMember gc
            ] & length
      in
        ic + oc

    cardViewGradeActualPriv__Student :: GClassroom -> Int
    cardViewGradeActualPriv__Student gc =
      let ic =
            [ (stud, grade)
            | stud <- gc & students
            , grade <- stud & getStudentGrades gc
            ] & length
          oc = 0
      in ic + oc

    -- requests
    randomPostAssignmentReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomPostAssignmentReqs gcDelta (gc, gcOld) = do
      let newTeachers = gcDelta & teachers
          newTAs      = gcDelta & tas
          (idealNum, overNum) =
            (gc & cardPostAssignmentActualPriv, gcOld & cardPostAssignmentActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ pPostAssignment gc newTeachers
      overRep  <- replicateM overNum  $ pPostAssignment gc newTAs
      return $ idealRep ++ overRep

    randomEditAssignmentReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomEditAssignmentReqs gcDelta (gc, gcOld) = do
      let newTeachers = gcDelta & teachers
          newTAs      = gcDelta & tas
          (idealNum, overNum) =
            (gc & cardEditAssignmentActualPriv, gcOld & cardEditAssignmentActualPriv)
            & calcNumSamples
      idealRep <- replicateM idealNum $ pEditAssignment gc newTeachers
      overRep  <- replicateM overNum  $ pEditAssignment gc newTAs
      return $ idealRep ++ overRep

    randomPostGradeReqs ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomPostGradeReqs gcDelta (gc, gcOld) = do
      let newStaff = gcDelta & getAllStaff
          -- no over privilege
          idealNum = gc & cardPostGradeActualPriv & repActual
      replicateM idealNum $ pPostGrade gc newStaff

    randomViewGradeReqs__Staff ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomViewGradeReqs__Staff gcDelta (gc, gcOld) = do
      let newStaff = gcDelta & getAllStaff
          (idealNum, overNum) =
            (gc & cardViewGradeActualPriv__Staff, gcOld & cardViewGradeActualPriv__Staff)
            & calcNumSamples
      idealRep <- replicateM idealNum $ ipViewGrade__Staff gc newStaff
      overRep  <- replicateM overNum  $ opViewGrade__Staff gc newStaff
      return $ idealRep ++ overRep

    randomViewGradeReqs__Student ::
      RandomGen g => GClassroom -> (GClassroom,GClassroom) -> State g [Request']
    randomViewGradeReqs__Student gcDelta (gc, gcOld) = do
      let newStuds = gcDelta & students
          idealNum = gc & cardViewGradeActualPriv__Student & repActual
      replicateM idealNum $ pViewGrade__Student gc newStuds

-- opPostAssignment :: RandomGen g => GClassroom -> State g Request'
-- opPostAssignment gc@GClassroom{..} = do
--   ta <- randomElem tas
--   assignment <- randomElem (ta & getAssignments gc)
--   return $ postAssignment ta assignment

-- vpEditAssignment :: RandomGen g => GClassroom -> State g Request'
-- vpEditAssignment gc@GClassroom{..} = do
--   teacher <- randomElem teachers
--   assignment <- randomElem (teacher & getAssignments gc)
--   return $ editAssignment teacher assignment

-- opEditAssignment :: RandomGen g => GClassroom -> State g Request'
-- opEditAssignment gc@GClassroom{..} = do
--   ta <- randomElem tas
--   assignment <- randomElem (ta & getAssignments gc)
--   return $ editAssignment ta assignment

-- vpPostGrade :: RandomGen g => GClassroom -> State g Request'
-- vpPostGrade gc@GClassroom{..} = do
--   staff <- randomElem (gc & getAllStaff)
--   grade <- randomElem (staff & getGrades gc)
--   return $ postGrade staff grade

-- vpStaffViewGrade :: RandomGen g => GClassroom -> State g Request'
-- vpStaffViewGrade gc@GClassroom{..} = do
--   staff <- randomElem (gc & getAllStaff)
--   grade <- randomElem (staff & getGrades gc)
--   return $ staffViewGrade staff grade

-- exercisedOverprivilegeSafe :: Config -> Int
-- exercisedOverprivilegeSafe GC{..} =
--   exercisedOverprivilege & max 0 & min 100

-- -- The teacher associated with the assignment posts it
-- pPostAssignment :: GClassroom -> Assignment -> Request'
-- pPostAssignment gc assign =
--   let teacher = assign & getTeacher gc in
--   postAssignment teacher assign

-- -- A random TA posts an assignment for their course (Not every course has a TA,
-- -- so we start with TAs here rather than assignments)
-- eopPostAssignment :: RandomGen g => GClassroom -> State g Request'
-- eopPostAssignment gc@GClassroom{..} = do
--   ta <- randomElem tas
--   course <- randomElem (ta & getCourses gc)
--   assignment <- randomElem (course & getAssignments gc)
--   return $ postAssignment ta assignment

-- -- The teacher associated with the assignment posts it
-- pEditAssignment :: GClassroom -> Assignment -> Request'
-- pEditAssignment gc assign =
--   let t = assign & getTeacher gc
--   in
--   editAssignment t assign

-- -- dEditAssignment :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- -- dEditAssignment gc assign = do
-- --   let c = getCourse gc assign
-- --   s <- randomElem (getStudents gc c)
-- --   return $ mkRequest' s (toAction EditAssignment) assign

-- -- A random TA edits an assignment for their course.
-- eopEditAssignment :: RandomGen g => GClassroom -> State g Request'
-- eopEditAssignment gc@GClassroom{..} = do
--   ta <- randomElem tas
--   course <- randomElem (ta & getCourses gc) -- ta & getStaffCourses gc & randomElem
--   assign <- randomElem (course & getAssignments gc)
--   return $ editAssignment ta assign

-- -- All grades will have a permitted post request, but the principal (course
-- -- staff) are randomly selected
-- --
-- -- TODO: This is a large amount of data, with diminishing benefit; perhaps this
-- -- should culled
-- pPostGrades :: RandomGen g => GClassroom -> Assignment -> State g [Request']
-- pPostGrades gc assign = do
--   let c = assign & getCourse gc
--   let staff@(teach:tas) = c & getStaff gc
--   let grades = assign & getGrades gc
--   forM grades
--     (\ gr -> do
--        principal <- randomElem staff
--        return $ postGrade principal gr)

-- -- -- For a given assignment, some students (for the course associated with the
-- -- -- assignment) try to post their own grades -- these are denied (by
-- -- -- default-deny)
-- -- dPostGrade :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- -- dPostGrade gc assign = do
-- --   let c = assign & getCourse gc
-- --   s <- randomElem (c & getStudents gc)
-- --   let g = findGrade gc assign s
-- --   return $ PostGrade & toAction & toRequest' s g

-- -- there are no over-privileges to exercise for posting grades


-- -- Viewing grades: after an assignment is released, a random student comes to
-- -- the office hours of a random course staff member, leading to the staff member
-- -- viewing the student's grade
-- --
-- -- For a given assignment, a random student and course staff (from the course
-- -- associated with the assignment). The staff member views the grade for the
-- -- (student,assignment) pair
-- pViewGrade__Staff :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- pViewGrade__Staff gc assign = do
--   let c = assign & getCourse gc
--   stud <- randomElem (c & getStudents gc)
--   staffMember <- randomElem (c & getStaff gc)
--   let gr = findGrade gc assign stud
--   return $ staffViewGrade staffMember gr

-- -- EOP: some staff request to view a grade for an assignment that is not for one
-- -- of their courses
-- eopViewGrade__Staff :: RandomGen g => GClassroom -> State g Request'
-- eopViewGrade__Staff gc@GClassroom{..} = do
--   -- pool of potential principals for this EOP: all staff for which there is at
--   -- least one course for which they are not in its staff
--   let eopPrincipals :: [(Staff, [Course])] =
--         gc
--         & getAllStaff
--         & map (\ staf -> (staf, courses & filter (notInCourse staf)))
--         & filter (\ (_, cs) -> length cs > 0)
--   -- randomly pick such a principal
--   (eopPrincipal, cs) <- randomElem eopPrincipals
--   -- randomly pick such a course
--   c <- randomElem cs
--   gr <- randomElem (c & getGrades gc)
--   return $ staffViewGrade eopPrincipal gr
--   where
--     notInCourse :: Staff -> Course -> Bool
--     notInCourse s c =
--       (s & uid) `notElem` (c & getStaff gc & map uid)

-- -- dViewGrades__StudentAssignment
-- --   :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- -- dViewGrades__StudentAssignment gc assign = do
-- --   let c = getCourse gc assign
-- --   s <- randomElem (getStudents gc c)
-- --   return $ mkRequest' s (toAction ViewGrades) assign

-- -- For a given assignment, randomly pick a student in the course for that
-- -- assignment; the student requests to view their grade for the assignment
-- pViewGrades__Student :: RandomGen g => GClassroom -> Assignment -> State g Request'
-- pViewGrades__Student gc@GClassroom{..} assign = do
--   let studs = assign & getStudents gc
--   stud <- randomElem studs
--   let gr = findGrade gc assign stud
--   return $ studentViewGrade stud gr

-- -- dViewGrades__StudentStudent
-- --   :: RandomGen g => GClassroom -> State g Request'
-- -- dViewGrades__StudentStudent GClassroom{..} = do
-- --   s1 <- randomElem students
-- --   s2 <- students & filter (\s -> uid s /= uid s1) & randomElem
-- --   return $ mkRequest' s1 (toAction ViewGrades) s2

-- createEventLog :: RandomGen g => Config -> GClassroom -> State g [Request']
-- createEventLog conf@GC{..} gc@GClassroom{..} = do
--   -- outermost list: sets of requests by assignment
--   -- middle list: lists of requests grouped by the policy they exercise
--   -- [Assignment1: [post],[edit1, edit2...],[postGrade1,postGrade2,...],...]
--   okReqs :: [[[Request']]] <- do
--     forM assignments
--       (\ assign -> do
--           let post = pPostAssignment gc assign
--           edits <- editAssignReqs assign
--           grades <- gradeAssignment assign
--           staffViews <- staffViewGrades assign
--           studViews <- studViewGrades assign
--           return $ [post]:edits:grades:staffViews:[studViews])

--   -- flatten requests so that the only grouping is by policy exericsed. This
--   -- way, we can calculate how many exercised overprivileges to generate
--   let okReqsByPolicy = okReqs & foldl1 (zipWith (++))
--   if exercisedOverprivilegeSafe conf == 0
--     then return $ okReqsByPolicy & concat
--     else do
--     let numEOPByPolicy :: [Int] =
--           okReqsByPolicy
--           & map (\reqs -> numExercisedOverPriv (length reqs) (exercisedOverprivilegeSafe conf))
--     eops <-
--       -- create a list of stateful actions by repeatedly applying the appropriate EOP;
--       -- then execute this list in sequence
--       numEOPByPolicy
--       & zipWith ($)
--           [ flip replicateM eopPostAssign -- TA posts an assignment
--           , flip replicateM eopEditAssign -- TA edits an assignment
--           , const (return [])             -- no overprivileges
--           , flip replicateM eopStaffVG    -- staff not-of-course views grade
--           , const (return [])             -- no overprivileges
--           ]
--       & sequence
--     return $
--       okReqsByPolicy
--       & zipWith (++) eops
--       & concat

--   where
--     -- approx. 1 in 4 assignments need updating after posting (not counting EOPs)
--     assignmentNeedsUpdatingDist :: [Int]
--     assignmentNeedsUpdatingDist = [0,0,0,1]

--     -- returns either empty or single-element list
--     editAssignReqs :: RandomGen g => Assignment -> State g [Request']
--     editAssignReqs assign = do
--       num <- randomElem assignmentNeedsUpdatingDist
--       return $ replicate num (pEditAssignment gc assign)

--     -- for each assignment, grades for all students are posted
--     gradeAssignment :: RandomGen g => Assignment -> State g [Request']
--     gradeAssignment assign = pPostGrades gc assign

--     -- a random number of visits during office hours, up to a third of the
--     -- enrollment size (but possibly with repeating student visits)
--     staffViewGrades :: RandomGen g => Assignment -> State g [Request']
--     staffViewGrades assign = do
--       let studs = assign & getStudents gc
--       num <- state $ randomR (0, length studs `div` 3)
--       replicateM num $
--         pViewGrade__Staff gc assign

--     -- similar to the above, but for students viewing their own grade
--     studViewGrades :: RandomGen g => Assignment -> State g [Request']
--     studViewGrades assign = do
--       let studs = assign & getCourse gc & getStudents gc
--       num <- state $ randomR (0, length studs `div` 3)
--       replicateM num $
--         pViewGrades__Student gc assign

--     eopPostAssign :: RandomGen g => State g Request'
--     eopPostAssign = eopPostAssignment gc

--     eopEditAssign :: RandomGen g => State g Request'
--     eopEditAssign = eopEditAssignment gc

--     eopStaffVG :: RandomGen g => State g Request'
--     eopStaffVG = eopViewGrade__Staff gc
