%%%-------------------------------------------------------------------
%%% File    : connection_manager.erl
%%% Author  : mijk_ <mijkenator@gmail.com>
%%% Description : 
%%%-------------------------------------------------------------------
-module(cp_test).

-compile(export_all).

do_sql_query(SqlSt) ->
    io:format("do_sql_query run -> ~p ~n", [SqlSt]),
    Guid = list_to_atom(integer_to_list(random:uniform(10000000000))),
    gen_server:cast(connection_manager, {do_sql_query, self(), Guid, SqlSt}),
    receive
        {ret_sql_query, Guid, Ret} ->
        io:format("sqlret: ~p -> ~p~n", [SqlSt, Ret])
    after 5000 ->
        io:format("sql time out ~p ~n", [Guid])
    end.
    
do_sql_tests() ->
    MyTest = fun(X) ->
        do_sql_query("select id, name from charity where id=" ++
            integer_to_list(X) ++ " order by mission, address2")
    end,
    lists:foreach(fun(X) -> MyTest(X) end, lists:seq(1, 20)).
    
do_mt_sql_tests() ->
    lists:foreach(fun(X) -> spawn(fun() -> do_sql_tests() end) end, lists:seq(1, 20)).