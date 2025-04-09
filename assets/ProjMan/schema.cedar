// Roles: Developer, Planner, Program Manager, Accountant
entity Role;
entity Project { manager: User } ;
entity User in [Project] { role: Role };

// program manager, accountant, project manager, planner
action ViewBudget
  appliesTo { principal: [User], resource: [Project] };
// program manager, accountant, project manager
action EditBudget
  appliesTo { principal: [User], resource: [Project] };

// everyone associated with a project
action ViewSchedule
  appliesTo { principal: [User], resource: [Project] };
// project/program manager
action EditSchedule
  appliesTo { principal: [User], resource: [Project] };

// project/program manager, developer, planner
action ViewAssets
  appliesTo { principal: [User], resource: [Project] };
// project manager, developer
action EditAssets
  appliesTo { principal: [User], resource: [Project] };

// everyone associated with a project
action ViewCalendar
  appliesTo { principal: [User], resource: [Project] };
// project manager, planner
action EditCalendar
  appliesTo { principal: [User], resource: [Project] };