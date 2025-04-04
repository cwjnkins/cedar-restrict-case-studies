{-# LANGUAGE RecordWildCards #-}
module GClassroom.Config where

import Data.Function

import Lib.GClassroom

import Config

numStudents :: Config -> [Int]
numStudents GC{..} =
  [ (fromIntegral s * student_ratio) & ceiling & max 1
  | s <- size ]

numTeachers :: Config -> [Int]
numTeachers GC{..} =
  [ (fromIntegral s * teacher_ratio) & floor & max 1
  | s <- size ]

numTAs :: Config -> [Int]
numTAs GC{..} =
  [ (fromIntegral s * ta_ratio) & floor & max 1
  | s <- size ]

maxAssignmentsPerCourse :: Config -> Int
maxAssignmentsPerCourse GC{..} =
  max_assignments_per_course
  & max 1

maxStudentCourseload :: Config -> Int
maxStudentCourseload GC{..} =
  max_student_courseload
  & max 1

maxTeacherCourseload :: Config -> Int
maxTeacherCourseload GC{..} =
  max_teacher_courseload
  & max 1

maxTACourseload :: Config -> Int
maxTACourseload GC{..} =
  max_ta_courseload
  & max 1

-- misc
gcFamilyToPool :: Family GClassroom -> Pools GClassroom
gcFamilyToPool = toPoolsGen mergeGClassroom
