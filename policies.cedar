// posting assignments
// tightening: only when not isTA
permit
  ( principal
  , action == Action::"PostNextAssignment"
  , resource )
  when { principal in resource };

// editing assignments
// tightening: only when not isTA
permit
  ( principal
  , action == Action::"EditAssignment"
  , resource )
  when { principal in resource.course };

// grading assignments
permit
  ( principal
  , action == Action::"GradeSubmission"
  , resource )
  when { principal in resource.course };

// viewing grades: staff
// tightening: only staff in the course can view
permit
  ( principal is Staff
  , action == Action::"ViewGrades"
  , resource is Assignment );

// viewing grades
permit
  ( principal is Student
  , action == Action::"ViewGrades"
  , resource is Student )
  when { principal == resource };

// permit
//   ( principal is Staff
//   , action == Action::"ViewGrades"
//   , resource )
//   when {    principal in context.course
//          && resource in context.course.students };

// permit
//   ( principal is Student
//   , action == Action::"ViewGrades"
//   , resource )
//   when { principal == resource };

