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
    initialize_model/3,
    initialize_model/4,
    add_layer/3,
    add_activation/3,
    set_cost/3,
    add_data_chunk/4,
    add_data_chunk/5,
    set_learning_rate/3,
    set_weights/3,
    train/2,
    train/3,
    train/4,
    get_weights/2,
    predict/3,
    disconnect/1,
    terminate_model/2
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
-define(TIMEOUT, 60000).

-spec start_link(network_id(), inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | ignore | {ok, pid()}.
start_link(NetworkId, Address, Port) ->
    gen_server:start_link(?NETWORK_GID(NetworkId), ?MODULE, [NetworkId, Address, Port], []).

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | {ok, network_id()}.
connect(Address, Port) ->
    annlink_model_sup:start_network(Address, Port).

%% ann helper function.
-spec create_neural_network(network_id(), [pos_integer()]) -> client_id().
create_neural_network(NetworkId, Layers) ->
    create_neural_network(NetworkId, Layers, sigmoid).

-spec create_neural_network(network_id(), [pos_integer()], atom() | matrix()) -> {error, term()} | client_id().
create_neural_network(_NetworkId, Layers, _) when length(Layers) < 3 ->
    {error, <<"Minimum number of layers is 3">>};
create_neural_network(NetworkId, Layers, Weights) when is_list(Weights) ->
    create_neural_network(NetworkId, Layers, Weights, sigmoid);
create_neural_network(NetworkId, [InputSize | Rest], Activation) when is_atom(Activation) ->
    case lists:member(Activation, supported_activations()) of
        false -> {error, <<"Activation function not supported">>};
        true ->
            %% TODO: This might will crash the caller change to handle errors.
            ClientId = initialize_model(NetworkId, InputSize, lists:last(Rest)),
            add_layers(NetworkId, ClientId, Rest, Activation),
            ClientId
    end.

-spec create_neural_network(network_id(), [pos_integer()], matrix(), atom()) -> {error, term()} | client_id().
create_neural_network(NetworkId, Layers, Weights, Activation) when is_atom(Activation) ->
    case create_neural_network(NetworkId, Layers, Activation) of
        {error, _} = Error -> Error;
        ClientId ->
            set_weights(NetworkId, ClientId, Weights),
            ClientId
    end.

-spec supported_activations() -> [atom()].
supported_activations() -> [relu, sigmoid, softsign, tanh, tanh1].

-spec initialize_model(network_id(), pos_integer(), pos_integer()) -> client_id().
initialize_model(NetworkId, InpSize, OutSize) ->
    initialize_model(NetworkId, InpSize, OutSize, ?LEARNING_RATE).

-spec initialize_model(network_id(), pos_integer(), pos_integer(), float()) -> client_id().
initialize_model(NetworkId, InpSize, OutSize, LearningRate) ->
    gen_server:call(?NETWORK_GID(NetworkId), {initialize_model, [InpSize, OutSize, LearningRate]}, ?TIMEOUT).

-spec add_layer(network_id(), client_id(), pos_integer()) -> ok | {error, binary()}.
add_layer(NetworkId, ClientId, Size) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_layer, [ClientId, Size]}, ?TIMEOUT).

-spec add_activation(network_id(), client_id(), atom()) -> ok.
add_activation(NetworkId, ClientId, Activation) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_activation, [ClientId, atom_to_binary(Activation, utf8)]}, ?TIMEOUT).

-spec set_cost(network_id(), client_id(), atom()) -> ok.
set_cost(NetworkId, ClientId, CostFunc) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_cost, [ClientId, atom_to_binary(CostFunc, utf8)]}, ?TIMEOUT).

-spec add_data_chunk(network_id(), client_id(), matrix(), matrix()) -> ok.
add_data_chunk(NetworkId, ClientId, Inputs, Labels) ->
    add_data_chunk(NetworkId, ClientId, Inputs, Labels, ?SCALE).

-spec add_data_chunk(network_id(), client_id(), matrix(), matrix(), matrix()) -> ok.
add_data_chunk(NetworkId, ClientId, Inputs, Labels, Scale) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_data_chunk, [ClientId, Inputs, Labels, Scale]}, ?TIMEOUT).

-spec set_learning_rate(network_id(), client_id(), float()) -> ok.
set_learning_rate(NetworkId, ClientId, LearningRate) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_learning_rate, [ClientId, LearningRate]}, ?TIMEOUT).

-spec train(network_id(), client_id()) -> float().
train(NetworkId, ClientId) ->
    train(NetworkId, ClientId, ?EPOCHS).

-spec train(network_id(), client_id(), pos_integer()) -> float().
train(NetworkId, ClientId, Epochs) when is_integer(Epochs) ->
    train(NetworkId, ClientId, Epochs, ?BATCH_SIZE).

-spec train(binary(), client_id(), pos_integer(), pos_integer()) -> float().
train(NetworkId, ClientId, Epochs, BatchSize) ->
    gen_server:call(?NETWORK_GID(NetworkId), {train, [ClientId, Epochs, BatchSize]}, ?TIMEOUT).

-spec get_weights(network_id(), client_id()) -> matrix().
get_weights(NetworkId, ClientId) ->
    gen_server:call(?NETWORK_GID(NetworkId), {get_weights, [ClientId]}, ?TIMEOUT).

-spec set_weights(network_id(), client_id(), matrix()) -> ok.
set_weights(NetworkId, ClientId, Weights) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_weights, [ClientId, Weights]}, ?TIMEOUT).

-spec predict(network_id(), client_id(), matrix()) -> matrix().
predict(NetworkId, ClientId, [N | _] = Input) when is_number(N) ->
    %% TODO: Check for errors...
    [Result] = predict(NetworkId, ClientId, [Input]),
    Result;
predict(NetworkId, ClientId, Set) ->
    gen_server:call(?NETWORK_GID(NetworkId), {predict, [ClientId, Set]}, ?TIMEOUT).

-spec terminate_model(network_id(), client_id()) -> ok.
terminate_model(NetworkId, ClientId) ->
    gen_server:call(?NETWORK_GID(NetworkId), {terminate_model, [ClientId]}, ?TIMEOUT).

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
-spec add_layers(network_id(), client_id(), [pos_integer()], atom()) -> ok.
add_layers(NetworkId, ClientId, [Size], _Activation) ->
    add_layer(NetworkId, ClientId, Size);
add_layers(NetworkId, ClientId, [Size | Rest], Activation) ->
    add_layer(NetworkId, ClientId, Size),
    add_activation(NetworkId, ClientId, Activation),
    add_layers(NetworkId, ClientId, Rest, Activation).
