// first assistance: separating policies into actions
permit
  ( principal is User
  , action == Action::"ViewBudget"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"EditBudget"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"ViewSchedule"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"EditSchedule"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"ViewAssets"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"EditAssets"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"ViewCalendar"
  , resource is Project )
  when { principal in resource };

permit
  ( principal is User
  , action == Action::"EditCalendar"
  , resource is Project )
  when { principal in resource };

