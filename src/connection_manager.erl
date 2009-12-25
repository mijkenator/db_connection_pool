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
  

handle_cast({command, make_pool},
    #connector_state{maxworkers = MaxWorkers, workermod = Module} = State) ->
    io:format("connection_manager -> make pool ~p ~p ~n", [MaxWorkers, Module]),
    lists:foreach(fun(X) ->
        connection_sup:start_client(Module,
            list_to_atom(string:concat("dbconnector",integer_to_list(X)))) end,
                lists:seq(1, MaxWorkers)),
    {noreply, State};
handle_cast({ret_sql_query, From, Guid, Ret}, State) ->
    From ! {ret_sql_query, Guid, Ret},
    {noreply, State};
handle_cast({ret_param_query, From, Guid, Ret}, State) ->
    From ! {ret_param_query, Guid, Ret},
    {noreply, State};
handle_cast({do_sql_query, From, Guid, SqlSt},
    #connector_state{ maxworkers = MaxWorkers } = State) ->
    gen_server:cast(get_connector(MaxWorkers) ,{do_sql_query, From, Guid, SqlSt}),
    {noreply, State};
handle_cast({do_param_query, From, Guid, SqlSt, Params},
    #connector_state{ maxworkers = MaxWorkers } = State) ->
    gen_server:cast(get_connector(MaxWorkers) ,{do_sql_query, From, Guid, SqlSt, Params}),
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


get_connector(MaxWorkers) ->
    list_to_atom(string:concat("dbconnector",
        integer_to_list(random:uniform(MaxWorkers)))).
        
smart_get_connector(MaxWorkers) ->
    {Qsize, RegName} = lists:min(lists:map(
        fun(X) ->
            RegName = list_to_atom(string:concat("dbconnector", integer_to_list(X))),
            {message_queue_len, Len} =
                process_info(whereis(RegName), message_queue_len),
            {Len, RegName}
        end,
        lists:seq(1, MaxWorkers))),
    RegName.


    