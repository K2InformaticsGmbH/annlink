-module(annlink_conn).

-include("annlink.hrl").

-export([
    connect/2,
    call/3
]).

-spec connect(inet:socket_address() | inet:hostname(), inet:port_number()) ->  {ok, term()} | {error, binary()}.
connect(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}, {packet, raw}]) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

-spec call(term(), binary(), [number()]) -> term() | {error, binary()}.
call(Socket, Operation, Arguments) ->
    MsgBin = jsx:encode(#{operation => Operation, args => Arguments}),
    MsgSize = byte_size(MsgBin),
    Msg = <<MsgSize:4/unit:8, MsgBin/binary>>,
    %% TODO: Check if this can crash by returning something else.
    ok = gen_tcp:send(Socket, Msg),
    get_result(Socket).

-spec get_result(term()) -> term() | {error, binary()}.
get_result(Socket) ->
    case gen_tcp:recv(Socket, 4) of
        {ok, <<MsgSize:4/unit:8>>} ->
            case gen_tcp:recv(Socket, MsgSize) of
                {ok, MsgBin} ->
                    case jsx:decode(MsgBin, [return_maps]) of
                        #{<<"error">> := Reason} when is_binary(Reason) -> {error, Reason};
                        #{<<"result">> := <<"ok">>} -> ok;
                        #{<<"result">> := Result} -> Result;
                        Invalid ->
                            ?Error("invalid response from server ~p", [Invalid]),
                            {error, <<"Invalid response from server">>}
                    end;
                {error, Reason} ->
                    {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
            end;
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.