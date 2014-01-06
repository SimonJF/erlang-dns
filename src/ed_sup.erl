%%%----------------------------------------------------------------------------
%%% @doc Erlang DNS (EDNS) root supervisor
%%% @author Hans Christian v. Stockhausen <hc@vst.io>
%%% @end
%%%----------------------------------------------------------------------------

-module(ed_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% behaviour callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Start the supervisor
%% @spec start_link() -> {ok, Pid::pid()}
%%-----------------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%============================================================================
%%% behaviour callbacks
%%%============================================================================
init([]) ->
  UdpServer = {ed_server, {ed_server, start_link, []},
    permanent, 2000, worker, [ed_server]},
  LookupSupervisor = {ed_lkup_sup, {ed_lkup_sup, start_link, []},
    permanent, 2000, supervisor, [ed_lkup_sup]},
  LookupManager = {ed_lkup_mngr, {ed_lkup_mngr, start_link, []},
    permanent, 2000, worker, [ed_lkup_mngr]},
  Children = [UdpServer, LookupSupervisor, LookupManager],
  RestartStrategy = {one_for_one, 3600, 4},
  {ok, {RestartStrategy, Children}}.
