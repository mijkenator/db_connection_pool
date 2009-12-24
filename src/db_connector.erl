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
        wokrnumber,
        configitem,
        url,
        counter=0}).
        
start_link(WorkerName) ->
    gen_server:start_link({local, WorkerName},?MODULE,
        #connector_state{workername = WorkerName, counter=0}, []).

init(Args) ->
  io:format("db connector init callback launched ~p ~n", [Args]),
  {ok, Args}.

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
