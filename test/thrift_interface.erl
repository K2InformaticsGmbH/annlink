%%%-------------------------------------------------------------------
%%% File        : thrift_interface.erl
%%% Description : Eunit tests regarding the Apache Thrift interface.
%%%
%%% Created     : 27.09.2017
%%%
%%% Copyright (c) 2012-17 K2 Informatics GmbH.  All Rights Reserved.
%%%-------------------------------------------------------------------

-module(thrift_interface).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Default values.
%%------------------------------------------------------------------------------

-define(ACTIVATION, sigmoid).
-define(COST_FUNCTION, scaled).
-define(HOST, '127.0.0.1').
-define(INPUT_SIZE, 2).
-define(LEARNING_RATE, 0.05).
-define(OUTPUT_SIZE, 1).
-define(PORT, 8778).

%%--------------------------------------------------------------------
%% ANN helper function tests.
%%--------------------------------------------------------------------

special_test() ->

    {ok, Conn} = annlink:connect(?HOST, ?PORT),

    %% Test case 1 -------------------------------------------------------------
    {error, <<"Minimum number of layers is 3">>} = annlink:create_neural_network(Conn, [2, 10]),

    %% Test case 2 -------------------------------------------------------------
    Weights = [
        [
            [0.01492280513048172, 0.007079346105456352, 0.007773277815431356],
            [0.0054452805779874325, 0.0035856086760759354, 0.0024345640558749437],
            [0.005277643445879221, 0.003077964298427105, 0.0015105109196156263],
            [-0.009498641826212406, -0.005496884696185589, -0.005575036164373159],
            [-0.010342562571167946, -0.006289885379374027, -0.005971433129161596],
            [-0.002944342093542218, -0.0024085245095193386, -0.0011626369087025523],
            [0.007942196913063526, 0.004708074498921633, 0.004946576431393623],
            [-0.0060704718343913555, -0.005143806338310242, -0.0040094489231705666],
            [-0.011893216520547867, -0.005822919309139252, -0.008928846567869186],
            [-0.008744233287870884, -0.003426315961405635, -0.004100374411791563]
        ],
        [
            [-0.04948689788579941, -0.013954956084489822, -7.015931187197566e-4,
                0.0013441058108583093, 0.0060188053175807, 0.00430743582546711,
                0.008657348342239857, -5.829081637784839e-4, 0.012651530094444752,
                -0.002089593093842268, 0.0032318499870598316]
        ]
    ],

    ClientId1 = annlink:create_neural_network(Conn, [2, 10, 1], Weights),

    ok = annlink:terminate_model(Conn, ClientId1),

    %% Test case 3 -------------------------------------------------------------
    {error, <<"Activation function not supported">>} = annlink:create_neural_network(Conn, [2, 10, 1], relux),

    %% Test case 4 -------------------------------------------------------------
    {error, <<"Activation function not supported">>} = annlink:create_neural_network(Conn, [2, 10, 1], Weights, relux),

    %% Test case 5 -------------------------------------------------------------
    ClientId2 = annlink:initialize_model(Conn, ?INPUT_SIZE, ?OUTPUT_SIZE, ?LEARNING_RATE),

    ok = annlink:add_layer(Conn, ClientId2, 10),

    ok = annlink:add_layer(Conn, ClientId2, ?OUTPUT_SIZE),

    ok = annlink:add_activation(Conn, ClientId2, ?ACTIVATION),

    Inputs = [[0, 0], [0, 1], [1, 0], [1, 1]],
    Labels = [[0], [1], [1], [0]],
    ok = annlink:add_data_chunk(Conn, ClientId2, Inputs, Labels, []),

    TrainResult = annlink:train(Conn, ClientId2),
    ?debugFmt(?MODULE_STRING ++ ":basic_test : client ~p ===> [Test case 5] training result #1:~n~p~n", [ClientId2, TrainResult]),

    Prediction = annlink:predict(Conn, ClientId2, [0, 0]),
    ?debugFmt(?MODULE_STRING ++ ":basic_test : client ~p ===> [Test case 5] prediction:~n~p~n", [ClientId2, Prediction]),

    ok = annlink:terminate_model(Conn, ClientId2),

    annlink:disconnect(Conn).

%%--------------------------------------------------------------------
%% Basic Apache Thrift interface tests.
%%--------------------------------------------------------------------

basic_test() ->

    {ok, Conn} = annlink:connect(?HOST, ?PORT),

    ClientId = annlink:initialize_model(Conn, ?INPUT_SIZE, ?OUTPUT_SIZE, ?LEARNING_RATE),

    ok = annlink:add_layer(Conn, ClientId, 10),

    ok = annlink:add_layer(Conn, ClientId, ?OUTPUT_SIZE),

    ok = annlink:add_activation(Conn, ClientId, ?ACTIVATION),

    Inputs = [[0, 0], [0, 1], [1, 0], [1, 1]],
    Labels = [[0], [1], [1], [0]],
    ok = annlink:add_data_chunk(Conn, ClientId, Inputs, Labels, []),

    ok = annlink:set_learning_rate(Conn, ClientId, ?LEARNING_RATE),

    TrainResult = annlink:train(Conn, ClientId),
    ?debugFmt(?MODULE_STRING ++ ":basic_test : client ~p ===> training result #1:~n~p~n", [ClientId, TrainResult]),

    ok = annlink:set_cost(Conn, ClientId, ?COST_FUNCTION),

    Weights = annlink:get_weights(Conn, ClientId),
    ?debugFmt(?MODULE_STRING ++ ":basic_test : client ~p ===> weights:~n~p~n", [ClientId, Weights]),

    ok = annlink:set_weights(Conn, ClientId, Weights),

    Prediction = annlink:predict(Conn, ClientId, [[0, 0], [0, 1], [1, 0], [1, 1]]),
    ?debugFmt(?MODULE_STRING ++ ":basic_test : client ~p ===> prediction:~n~p~n", [ClientId, Prediction]),

    ok = annlink:terminate_model(Conn, ClientId),

    annlink:disconnect(Conn).
