%%%-------------------------------------------------------------------
%% @doc game_svr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(game_svr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-include("com_def.hrl").
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    ChildSpecs = child_specs(),
    {ok, {{one_for_one, 10, 30}, ChildSpecs}}. % 10s内重启子进程30次

%%====================================================================
%% Internal functions
%%====================================================================

child_specs() -> 
  % Child不可以重启的监控树
  SupUnRst = ?CHILD(sup_unrst, worker),
  SupRst   = ?CHILD(sup_rst, worker),
  SupWs    = ?CHILD(sup_ws, worker),
  [SupUnRst, SupRst, SupAfter, SupWs].