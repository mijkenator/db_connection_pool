%%%-------------------------------------------------------------------
%%% File    : db_connection_pool.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(db_connection_pool).

-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) ->
    case connection_sup:start_link(10, db_connector) of
        {ok, Pid} ->
            io:format("Connection pool started OK ~n"),
            gen_server:cast(connection_manager, {command, make_pool}), {ok, Pid};
        Ret -> Ret
    end.

stop(_S) -> ok.




