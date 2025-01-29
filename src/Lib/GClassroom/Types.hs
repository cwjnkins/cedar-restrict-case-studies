{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecordWildCards #-}
module Lib.GClassroom.Types where

import Data.Aeson
import GHC.Generics

import Lib.Action
import Lib.Entity

type Student = Entity Value
mkStudent :: String -> Student
mkStudent name = Entity (mkUID "Student" name) (object []) []

data CourseAttrs = CourseAttrs { enrolled :: [UID] }
  deriving (Generic, Show)
instance ToJSON CourseAttrs where
  toJSON (CourseAttrs enrolled) =
    object ["students" .= enrolled]

type Course = Entity CourseAttrs
mkCourse :: String -> [Student] -> Course
mkCourse name studs =
  let studUIDs = map uid studs in
  Entity (mkUID "Course" name) (CourseAttrs studUIDs) []

getCourseEnrolled :: Course -> [UID]
getCourseEnrolled Entity{..} = enrolled attrs


data AssignmentAttrs = AssignmentAttrs { course :: UID }
  deriving (Generic, Show)
instance ToJSON AssignmentAttrs

type Assignment = Entity AssignmentAttrs
mkAssignment :: String -> Course -> Assignment
mkAssignment name course =
  Entity (mkUID "Assignment" name) (AssignmentAttrs (uid course)) []

getAssignmentCourse :: Assignment -> UID
getAssignmentCourse Entity{..} = uid

type Role = Entity Value
mkRole :: String -> Role
mkRole name =  Entity (mkUID "Role" name) (object []) []

roleTeacher :: Role
roleTeacher = mkRole "Teacher"

roleTA :: Role
roleTA = mkRole "TA"

data StaffAttrs = StaffAttrs { role :: UID }
  deriving (Generic, Show)
instance ToJSON StaffAttrs

type Staff = Entity StaffAttrs
mkStaff :: String -> Role -> [Course] -> Staff
mkStaff name role courses =
  Entity (mkUID "Staff" name) (StaffAttrs (uid role)) (map uid courses)

mkTeacher :: String -> [Course] -> Staff
mkTeacher name courses = mkStaff name roleTeacher courses

mkTA :: String -> [Course] -> Staff
mkTA name courses = mkStaff name roleTA courses

data GClassroom = GClassroom
  { students    :: [Student]
  , courses     :: [Course]
  , assignments :: [Assignment]
  , tas         :: [Staff]
  , teachers    :: [Staff]
  }

data GClassAction =
    PostNextAssignment
  | EditAssignment
  | GradeSubmission
  | ViewGrades

toAction :: GClassAction -> Action
toAction PostNextAssignment = Action "PostNextAssignment"
toAction EditAssignment = Action "EditAssignment"
toAction GradeSubmission = Action "GradeSubmission"
toAction ViewGrades = Action "ViewGrades"

data GClassEntity =
    GCCourse Course
  | GCRole Role
  | GCStaff Staff
  | GCStudent Student
  | GCAssignment Assignment
  deriving (Generic, Show)
instance ToJSON GClassEntity where
  toJSON (GCCourse course) = toJSON course
  toJSON (GCRole role)     = toJSON role
  toJSON (GCStaff staff)   = toJSON staff
  toJSON (GCStudent stud)  = toJSON stud
  toJSON (GCAssignment assign) = toJSON assign

toGClassEntities :: GClassroom -> [GClassEntity]
toGClassEntities gc =
     map GCRole [roleTeacher, roleTA]
  ++ map GCStudent (students gc)
  ++ map GCStaff (teachers gc ++ tas gc)
  ++ map GCCourse (courses gc)
  ++ map GCAssignment (assignments gc)
