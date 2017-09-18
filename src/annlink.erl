-module(annlink).
-behaviour(gen_server).

-include("annlink.hrl").

-export([
    start_link/3,
    new_connection/2,
    create_neural_network/2,
    create_neural_network/3,
    create_neural_network/4,
    supported_activations/0,
    initialize/3,
    add_layer/2,
    add_activation/2,
    set_weights/2,
    predict/2
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
    networkId :: binary(),
    conn :: term()
}).

-spec start_link(binary(), inet:socket_address() | inet:hostname(), inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(NetworkId, Address, Port) ->
    gen_server:start_link(?NETWORK_GID(NetworkId), ?MODULE, [NetworkId, Address, Port], []).

-spec new_connection(inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | {ok, binary()}.
new_connection(Address, Port) ->
    annlink_model_sup:start_network(Address, Port).

%% ann helper function.
-spec create_neural_network(binary(), [pos_integer()]) -> {error, term()} | {ok, binary()}.
create_neural_network(NetworkId, Layers) ->
    create_neural_network(NetworkId, Layers, []).

-spec create_neural_network(binary(), [pos_integer()], [float()] | atom()) -> {error, term()} | {ok, binary()}.
create_neural_network(_NetworkId, Layers, _) when length(Layers) < 3 ->
    {error, <<"Minimum number of layers is 3">>};
create_neural_network(NetworkId, Layers, Weights) when is_list(Weights) ->
    create_neural_network(NetworkId, Layers, Weights, sigmoid);
create_neural_network(NetworkId, [InputSize | Rest], Activation) when is_atom(Activation) ->
    case lists:member(Activation, supported_activations()) of
        false -> {error, <<"Activation function not supported">>};
        true ->
            %% TODO: This might will crash the caller change to handle errors.
            ok = initialize(NetworkId, InputSize, lists:last(Rest)),
            ok = add_layers(NetworkId, Rest, Activation),
            {ok, NetworkId}
    end.

-spec create_neural_network(binary(), [pos_integer()], [float()], atom()) -> {error, term()} | {ok, binary()}.
create_neural_network(NetworkId, Layers, Weights, Activation) when is_atom(Activation) ->
    case create_neural_network(NetworkId, Layers, Activation) of
        {error, _} = Error -> Error;
        {ok, NetworkId} ->
            ok = set_weights(NetworkId, Weights),
            {ok, NetworkId}
    end.

-spec supported_activations() -> [atom()].
supported_activations() -> [relu, sigmoid, softsign, tanh, tanh1].

-spec initialize(binary(), pos_integer(), pos_integer()) -> ok | {error, binary()}.
initialize(NetworkId, InpSize, OutSize) ->
    gen_server:call(?NETWORK_GID(NetworkId), {initialize, InpSize, OutSize}).

-spec add_layer(binary(), pos_integer()) -> ok | {error, binary()}.
add_layer(NetworkId, Size) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_layer, Size}).

-spec add_activation(binary(), atom()) -> ok | {error, binary()}.
add_activation(NetworkId, Activation) ->
    gen_server:call(?NETWORK_GID(NetworkId), {add_activation, Activation}).

-spec set_weights(binary(), [float()]) -> ok | {error, binary()}.
set_weights(NetworkId, Weights) ->
    gen_server:call(?NETWORK_GID(NetworkId), {set_weights, Weights}).

-spec predict(binary(), [number()]) -> list() | {error, binary()} .
predict(NetworkId, [N|_]=Input) when is_number(N) ->
    %% TODO: Check for errors...
    [Result] = gen_server:call(?NETWORK_GID(NetworkId), {predict, [Input]}),
    Result;
predict(NetworkId, Set) ->
    gen_server:call(?NETWORK_GID(NetworkId), {predict, Set}).

%% Gen server related functions

init([NetworkId, Address, Port]) ->
    %% TODO: This might will crash the caller change to handle errors.
    {ok, Conn} = annlink_conn:connect(Address, Port),
    {ok, #state{networkId = NetworkId, conn = Conn}}.


%% TODO: Define logging macros
handle_call({initialize, InpSize, OutSize}, _From, #state{conn = Conn} = State) ->
    Result = annlink_conn:call(Conn, <<"initialize">>, [InpSize, OutSize]),
    {reply, Result, State};
handle_call({add_layer, Size}, _From, #state{conn = Conn} = State) ->
    Result = annlink_conn:call(Conn, <<"add_layer">>, [Size]),
    {reply, Result, State};
handle_call({add_activation, Activation}, _From, #state{conn = Conn} = State) ->
    Result = annlink_conn:call(Conn, <<"add_activation">>, [Activation]),
    {reply, Result, State};
handle_call({set_weights, Weights}, _From, #state{conn = Conn} = State) ->
    Result = annlink_conn:call(Conn, <<"set_weights">>, [Weights]),
    {reply, Result, State};
handle_call({predict, Set}, _From, #state{conn = Conn} = State) ->
    Result = annlink_conn:call(Conn, <<"predict">>, [Set]),
    {reply, Result, State};
handle_call(Req, _From, #state{networkId = NetworkId} = State) ->
    ?Error("Invalid call request ~p received by player ~p process ~p", [NetworkId, Req, self()]),
    {reply, {error, <<"invalid request">>}, State}.

handle_cast(Request, #state{networkId = NetworkId} = State) ->
    ?Error("Unsolicited cast ~p for player ~p", [Request, NetworkId]),
    {noreply, State}.

handle_info(Info, #state{networkId = NetworkId} = State) ->
    ?Error("Unsolicited message ~p for player ~p", [Info, NetworkId]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal helper functions

%% ann helper function.
-spec add_layers(binary(), [pos_integer()], atom()) -> ok.
add_layers(NetworkId, [Size], _Activation) ->
    add_layer(NetworkId, Size);
add_layers(NetworkId, [Size | Rest], Activation) ->
    add_layer(NetworkId, Size),
    add_activation(NetworkId, Activation),
    add_layers(NetworkId, Rest, Activation).

