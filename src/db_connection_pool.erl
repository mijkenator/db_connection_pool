%%%-------------------------------------------------------------------
%%% File    : db_connection_pool.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(db_connection_pool).

-behaviour(application).

-export([start/2, stop/1]).
-export([init/1]).


start(_Type, _Args) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [10, db_connector]).

stop(_S) -> ok.

init([MaxWorkers, Module]) ->
    io:format("main supervisor init ~p ~n", [self()]),
   {ok, {
            {one_for_one, 1, 60},
            [
              { connection_sup,
                {supervisor, start_link, [{local, connection_sup}, ?MODULE, [Module]]},
                permanent,                               % Restart  = permanent | transient | temporary
                infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                supervisor,                              % Type     = worker | supervisor
                []                                       % Modules  = [Module] | dynamic
              }
              ,
              { connection_manager,
                {connection_manager, start_link, [MaxWorkers, Module]},
                permanent,                               
                infinity,                                
                supervisor,                              
                []                                      
              }
            ]
        }
    };
init([Module]) ->
    {ok,
        {
            {simple_one_for_one, 1, 60},
            [
              % worker
              {   undefined,                               % Id       = internal id
                  {Module, start_link,[]},                  % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.


