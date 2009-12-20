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
    io:format("connection_sup start ~p ~p ~n", [MaxWorkers, Module]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [MaxWorkers, Module]).
    
init([MaxWorkers, Module]) ->
    io:format("Main supervisor init ~p ~p ~p ~n", [self(), MaxWorkers, Module]),
    {ok,
        {one_for_one, 1, 60},
            [
              { connection_manager,
                {connection_manager, start_link, [MaxWorkers, Module]},
                permanent,                               % Restart  = permanent | transient | temporary
                infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                supervisor,                              % Type     = worker | supervisor
                []                                       % Modules  = [Module] | dynamic
              }
            ]
    }.
    
