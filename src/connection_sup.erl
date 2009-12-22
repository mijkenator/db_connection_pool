%%%-------------------------------------------------------------------
%%% File    : connection_sup.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(connection_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(MaxWorkers, Module) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxWorkers, Module]).
    
init([MaxWorkers, Module]) ->
    {ok,
        {
          {one_for_one, 1, 60},
            [
              { connection_manager,
                {connection_manager, start_link, [MaxWorkers, Module]},
                permanent,                               % Restart  = permanent | transient | temporary
                2000,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                worker,                              % Type     = worker | supervisor
                [connection_manager]                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };
init([Module]) ->
    {ok,
        {{simple_one_for_one, 1, 60},
            [
              % worker
              {   undefined,                               % Id       = internal id
                  {Module, start_link, []},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.    
