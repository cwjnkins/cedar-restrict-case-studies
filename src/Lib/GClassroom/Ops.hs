{-# LANGUAGE RecordWildCards #-}

module Lib.GClassroom.Ops where

import Data.Function
import Data.List
import Data.Maybe

import Lib.Entity
import Lib.GClassroom.Types

getCourse :: GClassroom -> Assignment -> Course
getCourse GClassroom{..} assign =
    courses
  & find (\c -> uid c == (assign & attrs & course))
  & fromJust

getTeacher :: GClassroom -> Course -> Staff
getTeacher GClassroom{..} course =
    teachers
  & find (\t -> uid course `elem` parents t)
  & fromJust

getStudents :: GClassroom -> Course -> [Student]
getStudents GClassroom{..} course =
    students
  & filter (\ s -> uid s `elem` getCourseEnrolled course)

getCourseStaff :: GClassroom -> Course -> (Staff,[Staff])
getCourseStaff gc@GClassroom{..} course =
  let s1 = getTeacher gc course
      s2 =
          tas
        & filter (\ ta -> head (parents ta) == uid course)
  in
  (s1, s2)
