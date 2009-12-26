%%%-------------------------------------------------------------------
%%% File    : connection_manager.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(connection_manager).

-export([start_link/2]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-compile(export_all).
-behaviour(gen_server).

-record(connector_state,
      { maxworkers,
        workermod,
        workers_list,
        limit_workers = 100 }).
    
start_link(MaxWorkers, Module) ->
    io:format("connection_manager started ~p ~p ~n", [MaxWorkers, Module]),
    gen_server:start_link({local, connection_manager},
        ?MODULE, #connector_state{maxworkers = MaxWorkers, workermod = Module}, []).

init(Args) ->
  io:format("connection_manager init callback launched ~p ~n", [Args]),
  {ok, Args}.
  

handle_cast({command, make_pool},
    #connector_state{maxworkers = MaxWorkers, workermod = Module} = State) ->
    io:format("connection_manager -> make pool ~p ~p ~n", [MaxWorkers, Module]),
    WorkersList = lists:map(
        fun(X) ->
            list_to_atom(string:concat("dbconnector",integer_to_list(X)))
        end, lists:seq(1, MaxWorkers)),
    lists:foreach(
        fun(X) ->
            connection_sup:start_client(Module, X)
        end, WorkersList),
    {noreply, State#connector_state{ workers_list = WorkersList }};
    
%handle_cast({command, {command, increase_workers}},
%    #connector_state{maxworkers = MaxWorkers, workermod = Module,
%       workers_list = WorkersList, limit_workers = LimitWorkers } = State) ->
%    io:format("increase workers list ~n"),
%    AddWorkers = fun(Begin, End) ->
%        AddWorkersList = lists:map(
%            fun(X) ->
%                list_to_atom(string:concat("dbconnector",integer_to_list(X)))
%            end, lists:seq(Begin, End)),
%        lists:foreach(
%            fun(X) ->
%                connection_sup:start_client(Module, X)
%            end, AddWorkersList),
%        AddWorkersList
%    end,
%    if
%        LimitWorkers >= length(WorkersList) + 10 ->
%        LimitWorkers =< length(WorkersList) -> false;
%        LimitWorkers < length(WorkersList) + 10  ->
%    end,
%    {noreply, State#connector_state{ workers_list = WorkersList }};
    
handle_cast({ret_sql_query, From, Guid, Ret}, State) ->
    From ! {ret_sql_query, Guid, Ret},
    {noreply, State};
    
handle_cast({ret_param_query, From, Guid, Ret}, State) ->
    From ! {ret_param_query, Guid, Ret},
    {noreply, State};
    
handle_cast({do_sql_query, From, Guid, SqlSt},
    #connector_state{ maxworkers = _MaxWorkers,
        workers_list = WorkersList, limit_workers = LimitWorkers } = State) ->
    gen_server:cast(smart_get_connector(WorkersList, LimitWorkers) ,
        {do_sql_query, From, Guid, SqlSt}),
    {noreply, State};
    
handle_cast({do_param_query, From, Guid, SqlSt, Params},
    #connector_state{ maxworkers = _MaxWorkers,
        workers_list = WorkersList, limit_workers = LimitWorkers } = State) ->
    gen_server:cast(smart_get_connector(WorkersList, LimitWorkers) ,
        {do_sql_query, From, Guid, SqlSt, Params}),
    {noreply, State};
    
handle_cast(Msg, State) ->
    io:format("connection_manager unknown cast !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}.

handle_call(_Msg, _Caller, State) ->
    io:format("connection_manager unknown call !!!! ~p ~p ~p ~n", [_Msg, _Caller, State]),
    {noreply, State}.
    
% These are just here to suppress warnings.    
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.

smart_get_connector(WorkersList, LimitWorkers) ->
    {Qsize, RegName} = lists:min(lists:map(
        fun(X) ->
            {message_queue_len, Len} =
                process_info(whereis(X), message_queue_len),
            {Len, X}
        end,
        WorkersList)),
    if
        Qsize > 4, LimitWorkers > length(WorkersList) ->
            gen_server:cast(connection_manager, {command, increase_workers})
    end,
    RegName.

    