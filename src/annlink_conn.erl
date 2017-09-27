-module(annlink_conn).

-include("annlink.hrl").

-define(NODEBUG, true).

-export([
    call/3,
    connect/2,
    disconnect/1
]).

-spec call(state(), bitstring(), list()) -> thrift_return_matrix() | thrift_return_precision() | thrift_return_void().
call(#state{conn = Conn} = State, <<"add_activation">> = Operation, [Activation])
    when is_atom(Activation) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), [atom_to_list(Activation)]),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"add_data_chunk">> = Operation, [Inputs, Labels, Scale] = Arguments)
    when is_list(Inputs), is_list(Labels), is_list(Scale) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"add_layer">> = Operation, [Size] = Arguments)
    when is_integer(Size) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"get_weights">> = Operation, [] = Arguments) ->
    {ConnNew, {ok, Result}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {Result, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"initialize">> = Operation, [InpSize, OutSize, LearningRate] = Arguments)
    when is_integer(InpSize), is_integer(OutSize), is_float(LearningRate) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"predict">> = Operation, [Set] = Arguments)
    when is_list(Set) ->
    {ConnNew, {ok, Result}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {Result, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"set_cost">> = Operation, [CostFunc])
    when is_atom(CostFunc) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), [atom_to_list(CostFunc)]),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"set_learning_rate">> = Operation, [LearningRate] = Arguments)
    when is_float(LearningRate) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"set_weights">> = Operation, [Weights] = Arguments)
    when is_list(Weights) ->
    {ConnNew, {ok, ok}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {ok, State#state{conn = ConnNew}};
call(#state{conn = Conn} = State, <<"train">> = Operation, [Epochs, BatchSize] = Arguments)
    when is_integer(Epochs), is_integer(BatchSize) ->
    {ConnNew, {ok, Result}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {Result, State#state{conn = ConnNew}}.

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) -> {ok, thrift_client_id()} | {error, binary()}.
connect(Address, Port) ->
    case thrift_client_util:new(Address, Port, erlang_python_services_thrift, []) of
        {ok, ClientId} -> {ok, ClientId};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

-spec disconnect(thrift_client_id()) -> ok.
disconnect(#state{conn = Conn} = _State) ->
    thrift_client:close(Conn).
