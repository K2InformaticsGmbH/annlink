-module(annlink_conn).

-include("annlink.hrl").

-define(NODEBUG, true).

-export([
    call/3,
    connect/2,
    disconnect/1
]).

-spec call(state(), bitstring(), list()) -> thrift_return_matrix() | thrift_return_precision() | thrift_return_void().
call(#state{conn = Conn} = State, Operation, Arguments)
    when is_binary(Operation), is_list(Arguments) ->
    {ConnNew, {ok, Result}} = thrift_client:call(Conn, binary_to_atom(Operation, utf8), Arguments),
    {Result, State#state{conn = ConnNew}}.

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) -> {ok, thrift_client_id()} | {error, binary()}.
connect(Address, Port) ->
    case thrift_client_util:new(Address, Port, erlang_python_services_thrift, []) of
        {ok, ThriftClientId} -> {ok, ThriftClientId};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

-spec disconnect(thrift_client_id()) -> ok.
disconnect(#state{conn = Conn} = _State) ->
    thrift_client:close(Conn).
