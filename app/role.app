{application, role,
 [{description, "role application!"},
  {id, "role"},
  {vsn, "0.1"},
  {modules, [role_app,role_sup,role_server]},
  {registered, []},
  {applications, [kernel, stdlib, sasl]},
  {mod, {role_app, []}},
  {env, []}
  ]}.
