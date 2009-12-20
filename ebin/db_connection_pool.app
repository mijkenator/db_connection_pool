{application, db_connection_pool,
 [
  {description, "db_connection_pool"},
  {vsn, "0.01"},
  {id, "db_connection_pool"},
  {modules,      [connection_manager, db_connector]},
  {registered,   [connection_sup, connection_manager_s, connector1]},
  {applications, [kernel, stdlib]},
  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {db_connection_pool, []}},
  {env, [
         {ums_root,     "/var/lib/ums" }
        ]}
 ]
}.