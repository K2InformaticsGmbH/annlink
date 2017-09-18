%%%-------------------------------------------------------------------
%% @doc annlink top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(annlink_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), #{id => I, start => {I, start_link, []},
                                restart => permanent, shutdown => 5000,
                                type => Type, modules => [I]}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok
    , { {one_for_one, 5, 10}
      , [ ?CHILD(annlink_model_sup, supervisor)
        ]
      }
    }.

%%====================================================================
%% Internal functions
%%====================================================================
