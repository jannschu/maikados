{application, maikados,
 [{description, "Multiplayer server for the Maikados game"},
  {vsn, "0.1"},
  {modules, [maikados_app]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {maikados_app, []}}
 ]}.