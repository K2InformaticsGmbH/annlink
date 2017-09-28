-module(annlink).
-behaviour(gen_server).

-include("annlink.hrl").

-export([
    start_link/3,
    connect/2,
    create_neural_network/2,
    create_neural_network/3,
    create_neural_network/4,
    supported_activations/0,
    initialize/3,
    initialize/4,
    add_layer/2,
    add_activation/2,
    set_cost/2,
    add_data_chunk/3,
    add_data_chunk/4,
    set_learning_rate/2,
    set_weights/2,
    train/1,
    train/2,
    train/3,
    get_weights/1,
    predict/2,
    disconnect/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    networkId :: network_id(),
    conn :: connection_id()
}).

%% Default network values.
-define(LEARNING_RATE, 0.001).
-define(BATCH_SIZE, 512).
-define(EPOCHS, 1).
-define(SCALE, []).

-spec start_link(network_id(), inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | ignore | {ok, pid()}.
start_link(NetworkId, Address, Port) ->
    gen_server:start_link(?NETWORK_GID(NetworkId), ?MODULE, [NetworkId, Address, Port], []).

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | {ok, network_id()}.
connect(Address, Port) ->
    annlink_model_sup:start_network(Address, Port).

%% ann helper function.
-spec create_neural_network(network_id(), [pos_integer()]) -> ok.
create_neural_network(NetworkId, Layers) ->
    create_neural_network(NetworkId, Layers, sigmoid).

-spec create_neural_network(network_id(), [pos_integer()], atom() | matrix()) -> {error, term()} | ok.
create_neural_network(_NetworkId, Layers, _) when length(Layers) < 3 ->
    {error, <<"Minimum number of layers is 3">>};
create_neural_network(NetworkId, Layers, Weights) when is_list(Weights) ->
    create_neural_network(NetworkId, Layers, Weights, sigmoid);
create_neural_network(NetworkId, [InputSize | Rest], Activation) when is_atom(Activation) ->
    case lists:member(Activation, supported_activations()) of
        false -> {error, <<"Activation function not supported">>};
        true ->
            %% TODO: This might will crash the caller change to handle errors.
            ok = initialize(NetworkId, InputSize, lists:last(Rest)),
            add_layers(NetworkId, Rest, Activation)
    end.

-spec create_neural_network(network_id(), [pos_integer()], matrix(), atom()) -> {error, term()} | ok.
create_neural_network(NetworkId, Layers, Weights, Activation) when is_atom(Activation) ->
    case create_neural_network(NetworkId, Layers, Activation) of
        {error, _} = Error -> Error;
        ok -> set_weights(NetworkId, Weights)
    end.

-spec supported_activations() -> [atom()].
supported_activations() -> [relu, sigmoid, softsign, tanh, tanh1].

-spec initialize(network_id(), pos_integer(), pos_integer()) -> ok.
initialize(NetworkId, InpSize, OutSize) ->
    initialize(NetworkId, InpSize, OutSize, ?LEARNING_RATE).

-spec initialize(network_id(), pos_integer(), pos_integer(), float()) -> ok.
initialize(NetworkId, InpSize, OutSize, LearningRate) ->
    gen_server:call(?NETWORK_GID(NetworkId), {initialize, [InpSize, OutSize, LearningRate]}).

-spec add_layer(binary(), pos_integer()) -> ok | {error, binary()}.
add_layer(NetworkId, Size) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_layer, [Size]}).

-spec add_activation(network_id(), atom()) -> ok.
add_activation(NetworkId, Activation) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_activation, [atom_to_binary(Activation, utf8)]}).

-spec set_cost(network_id(), atom()) -> ok.
set_cost(NetworkId, CostFunc) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_cost, [atom_to_binary(CostFunc, utf8)]}).

-spec add_data_chunk(network_id(), matrix(), matrix()) -> ok.
add_data_chunk(NetworkId, Inputs, Labels) ->
    add_data_chunk(NetworkId, Inputs, Labels, ?SCALE).

-spec add_data_chunk(network_id(), matrix(), matrix(), matrix()) -> ok.
add_data_chunk(NetworkId, Inputs, Labels, Scale) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_data_chunk, [Inputs, Labels, Scale]}).

-spec set_learning_rate(network_id(), float()) -> ok.
set_learning_rate(NetworkId, LearningRate) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_learning_rate, [LearningRate]}).

-spec train(network_id()) -> float().
train(NetworkId) -> train(NetworkId, ?EPOCHS).

-spec train(network_id(), pos_integer()) -> float().
train(NetworkId, Epochs) when is_integer(Epochs) ->
    train(NetworkId, Epochs, ?BATCH_SIZE).

-spec train(binary(), pos_integer(), pos_integer()) -> float().
train(NetworkId, Epochs, BatchSize) ->
    gen_server:call(?NETWORK_GID(NetworkId), {train, [Epochs, BatchSize]}).

-spec get_weights(network_id()) -> matrix().
get_weights(NetworkId) ->
    gen_server:call(?NETWORK_GID(NetworkId), {get_weights, []}).

-spec set_weights(network_id(), matrix()) -> ok.
set_weights(NetworkId, Weights) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_weights, [Weights]}).

-spec predict(network_id(), matrix()) -> matrix().
predict(NetworkId, [N | _] = Input) when is_number(N) ->
    %% TODO: Check for errors...
    [Result] = predict(NetworkId, [Input]),
    Result;
predict(NetworkId, Set) ->
    gen_server:call(?NETWORK_GID(NetworkId), {predict, [Set]}).

-spec disconnect(network_id()) -> ok.
disconnect(NetworkId) ->
    annlink_model_sup:close_network(NetworkId).

%% Gen server related functions

-spec init(maybe_improper_list()) -> {ok, #state{}}.
init([NetworkId, Address, Port]) ->
    %% TODO: This might will crash the caller change to handle errors.
    {ok, Conn} = annlink_conn:connect(Address, Port),
    {ok, #state{networkId = NetworkId, conn = Conn}}.

handle_call({Operation, Args}, _From, #state{conn = Conn} = State) when is_atom(Operation) ->
    {Result, ConnNew} = annlink_conn:call(Conn, Operation, Args),
    {reply, Result, State#state{conn = ConnNew}};
handle_call(Req, _From, State) ->
    ?Error("Invalid call request ~p received by player ~p process ~p", [State, Req, self()]),
    {reply, {error, <<"invalid request">>}, State}.

handle_cast(Request, #state{networkId = NetworkId} = State) ->
    ?Error("Unsolicited cast ~p for player ~p", [Request, NetworkId]),
    {noreply, State}.

handle_info(Info, #state{networkId = NetworkId} = State) ->
    ?Error("Unsolicited message ~p for player ~p", [Info, NetworkId]),
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    annlink_conn:disconnect(Conn).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal helper functions

%% ann helper function.
-spec add_layers(network_id(), [pos_integer()], atom()) -> ok.
add_layers(NetworkId, [Size], _Activation) ->
    add_layer(NetworkId, Size);
add_layers(NetworkId, [Size | Rest], Activation) ->
    add_layer(NetworkId, Size),
    add_activation(NetworkId, Activation),
    add_layers(NetworkId, Rest, Activation).
