%%%-------------------------------------------------------------------
%%% @contrib gerald.xv
%%% @copyright (C) Lilith Games
%%% @doc
%%% websocket 监控树
%%% @end
%%%-------------------------------------------------------------------
-module(sup_ws).
-behaviour(supervisor).

-export([start_link/0, init/1]).

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
  WsHandler = tools:child_spec(ws_handler, start_handler, []),
  [WsHandler].