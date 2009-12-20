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
    supervisor:start_link(connection_manager_s, [MaxWorkers, Module]).
    
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
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
    }.
%init([Module]) ->
%    {ok,
%        {
%            {simple_one_for_one, 1, 60},
%            [
%              % worker
%              {   undefined,                               % Id       = internal id
%                  {Module, start_link,[connectorN]},                  % StartFun = {M, F, A}
%                  temporary,                               % Restart  = permanent | transient | temporary
%                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
%                  worker,                                  % Type     = worker | supervisor
%                  []                                       % Modules  = [Module] | dynamic
%              }
%            ]
%        }
%    }.    
    
