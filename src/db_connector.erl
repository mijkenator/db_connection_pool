%%%-------------------------------------------------------------------
%%% File    : connection_manager.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(db_connector).
-author('mijkenator@gmail.com').

-export([start_link/1]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-compile(export_all).

-behaviour(gen_server).

-record(connector_state,
    {   workername,
        reconnect_count=0,
        dbh,
        counter=0}).
        
start_link(WorkerName) ->
    gen_server:start_link({local, WorkerName},?MODULE,
        #connector_state{workername = WorkerName}, []).

init(Args) ->
  io:format("db connector init callback launched ~p ~n", [Args]),
  {ok, Ref} = odbc:connect("DSN=wish;UID=wish;PWD=wish", []),
  {ok, Args#connector_state{dbh = Ref}}.


handle_cast({do_sql_query, From, Guid, SqlSt},
    #connector_state{dbh = Ref, reconnect_count= _Rc,
    workername = WorkerName, counter = Counter} = State) ->
    Ret = odbc:sql_query(Ref, SqlSt),
    io:format("do_sql_query ret ~p -> ~p ~n", [WorkerName, Ret]),
    gen_server:cast(connection_manager, {ret_sql_query, From, Guid, Ret}), 
    {noreply, State#connector_state{counter = Counter + 1}};
    
handle_cast({do_param_query, From, Guid, SqlSt, Params},
    #connector_state{dbh = Ref, reconnect_count= _Rc, counter = Counter} = State) ->
    io:format("do_param_query  cast !!!! ~p ~p ~p ~n", [Guid, SqlSt, Params]),
    Ret = odbc:param_query(Ref, SqlSt, Params),
    io:format("do_param_query  ret ~p ~n", [Ret]),
    gen_server:cast(connection_manager, {ret_param_query, From, Guid, Ret}), 
    {noreply, State#connector_state{counter = Counter + 1}};
    
handle_cast(Msg, State) ->
    io:format("db connector unknown cast !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}.

handle_call(_Msg, _Caller, State) ->
    io:format("db connector unknown call !!!! ~p ~p ~p ~n", [_Msg, _Caller, State]),
    {noreply, State}.
    
% These are just here to suppress warnings.    
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
