%%%-------------------------------------------------------------------
%%% File        : readme.erl
%%% Description : Eunit tests based on the example of the README.md file.
%%%
%%% Created     : 25.09.2017
%%%
%%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
%%%-------------------------------------------------------------------

-module(readme).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Default values.
%%------------------------------------------------------------------------------

-define(HOST, '127.0.0.1').
-define(PORT, 8778).

%%--------------------------------------------------------------------
%% TEST CASES: README.md
%%--------------------------------------------------------------------

readme_test() ->
    ?debugFmt("~n" ++ ?MODULE_STRING ++ ": readme_test() ===> Start ~n", []),

    application:ensure_all_started(annlink),

    {ok, Conn} = annlink:connect(?HOST, ?PORT),

    ?debugFmt("~n" ++ ?MODULE_STRING ++ ": readme_test() ===> Conn=~p ~n", [Conn]),

    timer:sleep(1),
    ClientId = erlang:monotonic_time(nanosecond),
    ?debugFmt("~n" ++ ?MODULE_STRING ++ ": readme_test() ===> ClientId=~p ~n", [ClientId]),

    ok = annlink:create_neural_network(Conn, ClientId, [2, 10, 1]),

    Inputs = [[0, 0], [0, 1], [1, 0], [1, 1]],
    Labels = [[0], [1], [1], [0]],
    ok = annlink:add_data_chunk(Conn, ClientId, Inputs, Labels, []),

    ok = annlink:set_learning_rate(Conn, ClientId, 0.05),

    TrainResult = annlink:train(Conn, ClientId),
    ?debugFmt("~n" ++ ": readme_test ===> training result #1:~n~p~n", [TrainResult]),

    TrainResults = [annlink:train(Conn, ClientId, 200) || _ <- lists:seq(1, 5)],
    ?debugFmt("~n" ++ ": readme_test ===> remaining training results:~n~p~n", [TrainResults]),

    Prediction = annlink:predict(Conn, ClientId, [[0, 0], [0, 1], [1, 0], [1, 1]]),
    ?debugFmt("~n" ++ ": readme_test ===> prediction:~n~p~n", [Prediction]),

    ok = annlink:terminate_model(Conn, ClientId),

    annlink:disconnect(Conn).
