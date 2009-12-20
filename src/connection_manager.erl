%%%-------------------------------------------------------------------
%%% File    : connection_manager.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(connection_manager).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(MaxWorkers, Module) ->
    io:format("manager supervisor stl ~p ~p ~p ~n", [self(), MaxWorkers, Module]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxWorkers, Module]).
    
init([MaxWorkers, Module]) ->
    io:format("manager supervisor init ~p ~p ~p ~n", [self(), MaxWorkers, Module]),
    {ok,
        {one_for_one, 1, 60},
            [
              % worker
              {   connector1,                              % Id       = internal id
                  {Module, start_link, [connector1]},      % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  brutal_kill,                             % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [Module]                                       % Modules  = [Module] | dynamic
              }
            ]
    }.
    
