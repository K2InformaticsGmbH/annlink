-module(annlink_model_sup).
-behaviour(supervisor).

-include("annlink.hrl").

%% API
-export([start_link/0, start_network/2, close_network/1, list_networks/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, temporary, 5000, worker, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
    ?Info("~p starting...~n", [?MODULE]),
    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, _} = Success ->
            ?Info("~p started!~n", [?MODULE]),
            Success;
        Error ->
            ?Error("~p failed to start ~p~n", [?MODULE, Error]),
            Error
    end.

-spec start_network(inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, term()} | {ok, network_id()}.
start_network(Address, Port) ->
    NetworkId = base64:encode(crypto:strong_rand_bytes(24)),
    case supervisor:start_child(?MODULE, [NetworkId, Address, Port]) of
        {error, _} = Error -> Error;
        {ok, _NetworkPid} -> {ok, NetworkId}
    end.

-spec close_network(network_id()) -> ok | {error, not_found | simple_one_for_one}.
close_network(NetworkPid) when is_pid(NetworkPid) ->
    supervisor:terminate_child(?MODULE, NetworkPid);
close_network(NetworkId) ->
    {global, Name} = ?NETWORK_GID(NetworkId),
    case global:whereis_name(Name) of
        undefined -> {error, not_found};
        Pid -> supervisor:terminate_child(?MODULE, Pid)
    end.

-spec list_networks() -> [restarting | undefined | pid()].
list_networks() ->
    [Pid || {undefined, Pid, worker, _} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = {simple_one_for_one, 5, 10},
    {ok, {SupFlags, [?CHILD(annlink)]}}.
