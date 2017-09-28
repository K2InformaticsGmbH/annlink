-module(annlink_conn).

-include("annlink.hrl").

-define(NODEBUG, true).

-export([
    call/3,
    connect/2,
    disconnect/1
]).

-spec call(connection_id(), atom(), list()) -> {ok | float() | matrix(), connection_id()}.
call(Conn, Operation, Arguments)
    when is_atom(Operation), is_list(Arguments) ->
    {ConnNew, {ok, Result}} = thrift_client:call(Conn, Operation, Arguments),
    {Result, ConnNew}.

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) -> {error, binary()} | {ok, connection_id()}.
connect(Address, Port) ->
    case thrift_client_util:new(Address, Port, erlang_python_services_thrift, []) of
        {ok, ThriftClientId} -> {ok, ThriftClientId};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

-spec disconnect(connection_id()) -> ok.
disconnect(Conn) ->
    thrift_client:close(Conn).
