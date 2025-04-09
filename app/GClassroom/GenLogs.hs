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
