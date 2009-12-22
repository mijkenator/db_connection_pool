%%%-------------------------------------------------------------------
%%% File    : db_connection_pool.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(db_connection_pool).

-behaviour(application).

-export([start/2, stop/1]).


start(_Type, _Args) -> connection_sup:start_link(10, db_connector).

stop(_S) -> ok.




