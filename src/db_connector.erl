%%%-------------------------------------------------------------------
%%% File    : connection_manager.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(db_connector).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1]).

%% FSM States
-export([
    'WAIT_FOR_JOB'/2,
    'WAIT_FOR_WORK'/2
]).
-compile(export_all).

-behaviour(gen_fsm).

-record(connector_state,
    {   workername,
        wokrnumber,
        configitem,
        url,
        counter=0}).
        
start_link(WorkerName) ->
    io:format("db connector started ~p ~n", [WorkerName]),
    Result = gen_fsm:start_link({local, WorkerName},?MODULE, #connector_state{workername = WorkerName, counter=0}, []),
    io:format("db connector started Result ~p ~p ~n", [Result, WorkerName]),
    Result;
start_link([M]) -> io:format("db connector started 11111 ~p ~n", [M]).

init(Args) ->
  io:format("db connector init callback launched ~p ~n", [Args]),
  {ok, 'WAIT_FOR_JOB', Args, 10}.

'WAIT_FOR_JOB'(_Other, State) ->
    io:format("wait for job ~p ~n", [State]),
    {next_state, 'WAIT_FOR_JOB', State, 5000}.
    
'WAIT_FOR_WORK'(_Other, State) ->
    io:format("wait for work ~p ~n", [State]),
    {next_state, 'WAIT_FOR_WORK', State, 5000}.


handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
    
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.
    
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.
    
handle_info(Info, StateName, StateData) ->
    io:format("nexpected message ~p ~p ~p ~n", [Info, StateName, StateData]),
    {stop, normal, StateData}.
    
terminate(_Reason, _StateName, _State) ->
    io:format("Terminate ~n"),
    ok.
