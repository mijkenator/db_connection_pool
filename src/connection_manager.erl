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
    {   workername,
        wokrnumber,
        configitem,
        maxworkers,
        workermod,
        url,
        counter=0}).
    
start_link(MaxWorkers, Module) ->
    io:format("connection_manager started ~p ~p ~n", [MaxWorkers, Module]),
    gen_server:start_link({local, connection_manager},
        ?MODULE, #connector_state{maxworkers = MaxWorkers, workermod = Module}, []).

init(Args) ->
  io:format("connection_manager init callback launched ~p ~n", [Args]),
  {ok, Args}.
  
handle_cast(Msg, State) ->
    io:format("connection_manager unknown cast !!!! ~p ~p ~n", [Msg, State]),
    {noreply, State}.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


    