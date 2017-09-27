-ifndef(_ANNLINK_HRL_).
-define(_ANNLINK_HRL_, true).

%% Log helper macros (adapted from dderl)
%% https://github.com/K2InformaticsGmbH/dderl/blob/1.9.0/src/dderl.hrl

-ifndef(LOG_TAG).
-define(LOG_TAG, "_ANNLNK_").
-endif.

-define(Log(__L,__F,__A),
        lager:__L("["++?LOG_TAG++"] ~p "++__F, [{?MODULE,?LINE}|__A])).

-define(Debug(__F,__A),         ?Log(debug, __F,__A)).
-define(Info(__F,__A),          ?Log(info,__F,__A)).
-define(Note(__F, __A),         ?Log(notice, __F, __A)).
-define(Warn(__F,__A),          ?Log(warning,__F,__A)).
-define(Error(__F,__A),         ?Log(error,__F,__A)).
-define(Crit(__F,__A),          ?Log(critical,__F,__A)).
-define(Alert(__F,__A),         ?Log(alert,__F,__A)).
-define(Emergency(__F,__A),     ?Log(emergency,__F,__A)).

-define(Debug(__F),             ?Debug(__F,[])).
-define(Info(__F),              ?Info(__F,[])).
-define(Note(__F),              ?Note(__F, [])).
-define(Warn(__F),              ?Warn(__F,[])).
-define(Error(__F),             ?Error(__F,[])).
-define(Crit(__F),              ?Crit(__F,[])).
-define(Alert(__F),             ?Alert(__F,[])).
-define(Emergency(__F),         ?Emergency(__F,[])).


%% End Log helper macros.

-define(NETWORK_GID(__NetworkId), {global, {network, __NetworkId}}).

-endif.

-record(state, {
        networkId :: binary(),
        conn :: term()
}).

%%------------------------------------------------------------------------------
%% dialyzer support definitions.
%%------------------------------------------------------------------------------

-type matrix() :: [[number()]].
-type network_id() :: binary().
-type standard_error() :: {error, term()}.
-type state() :: #state{}.
-type thrift_client_id() :: {atom(), atom(), tuple(), pos_integer()}.
-type thrift_return_matrix() :: {matrix(), state()}.
-type thrift_return_precision() :: {float(), state()}.
-type thrift_return_precision_error() :: thrift_return_precision() | standard_error().
-type thrift_return_void() :: {ok, state()}.
-type thrift_return_void_error() :: thrift_return_void() | standard_error().
