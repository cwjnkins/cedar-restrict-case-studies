{-# LANGUAGE FlexibleInstances, RecordWildCards #-}

module Lib.GClassroom.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.GClassroom.Types

-- sloppily re-inventing lenses...

-- non-GC operations
findAssignmentCourse :: [Course] -> Assignment -> Course
findAssignmentCourse courses assignment =
  courses
  & find (\ c -> (c & uid) == (assignment & attrs & course))
  & fromJust

findCourseStudents :: [Student] -> Course -> [Student]
findCourseStudents students course =
  students
  & filter (\ s -> course `entityElem` (s & parents))

-- GC operations (for log generation)
class HasCourse a where
  getCourse :: GClassroom -> a -> Course

instance HasCourse Assignment where
  getCourse GClassroom{..} assignment =
    assignment & findAssignmentCourse courses

class HasCourses a where
  getCourses :: GClassroom -> a -> [Course]

instance HasCourses Staff where
  getCourses GClassroom{..} staff =
    courses
    & filter (\ c -> c `entityElem` (staff & parents))

class HasStaff a where
  getStaffDistinguished :: GClassroom -> a -> (Staff,[Staff])

getStaff :: HasStaff a => GClassroom -> a -> [Staff]
getStaff gc x = x & getStaffDistinguished gc & uncurry (:)

getTeacher :: HasStaff a => GClassroom -> a -> Staff
getTeacher gc x = x & getStaffDistinguished gc & fst

instance HasStaff Course where
  getStaffDistinguished GClassroom{..} course =
    let s1 =
          teachers
          & find (\ t -> course `entityElem` (t & parents))
          & fromJust
        s2 =
          tas
          & filter (\ t -> course `entityElem` (t & parents))
    in
    (s1, s2)

instance HasStaff Assignment where
  getStaffDistinguished gc assignment =
    assignment & getCourse gc & getStaffDistinguished gc

class HasStudents a where
  getStudents :: GClassroom -> a -> [Student]

instance HasStudents Course where
  getStudents GClassroom{..} course =
      students
    & filter (\ s -> course `entityElem` (s & parents))

instance HasStudents Assignment where
  getStudents gc assignment =
    assignment
    & getCourse gc
    & getStudents gc

getAssignments :: GClassroom -> Course -> [Assignment]
getAssignments GClassroom{..} c =
  assignments
  & filter (\ a -> (c & uid) == (a & attrs & course))

class HasGrades a where
  getGrades :: GClassroom -> a -> [Grade]

instance HasGrades Assignment where
  getGrades GClassroom{..} assignment =
    grades
    & filter (\ grade -> (grade & attrs & gassignment) == (assignment & uid))

instance HasGrades Course where
  getGrades gc@GClassroom{..} course =
    course
    & getAssignments gc
    & concatMap (getGrades gc)

findGrade :: GClassroom -> Assignment -> Student -> Grade
findGrade gc@GClassroom{..} assign stud =
  assign
  & getGrades gc
  & find (\ gr -> (gr & attrs & student) == (stud & uid))
  & fromJust

getAllStaff :: GClassroom -> [Staff]
getAllStaff GClassroom{..} = teachers ++ tas

getStudentGrades :: GClassroom -> Student -> [Grade]
getStudentGrades GClassroom{..} stud =
  grades
  & filter (\ gr -> (stud & uid) == (gr & attrs & student))
