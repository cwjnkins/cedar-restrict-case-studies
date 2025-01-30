permit
  ( principal is User
  , action
  , resource is Project )
  when { principal in resource };