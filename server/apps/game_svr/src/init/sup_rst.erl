%%%-------------------------------------------------------------------
%%% @contrib gerald.xv
%%% @copyright (C) Lilith Games
%%% @doc
%%% 可以重启内部模块的监控树
%%% @end
%%%-------------------------------------------------------------------
-module(sup_rst).
-behaviour(supervisor).

-include("com_def.hrl").

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
  % Monitor        = ?CHILD(monitor),
  % SlowDBSup      = ?CHILD(slow_db_sup),
  % MailSup        = ?CHILD(mail_sup),
  RedisSup       = ?CHILD(redis_sup),
  EhttpcSup      = ?CHILD(ehttpc_sup),
  LruSup         = ?CHILD(sup_lru),
  % VSvrSups       = [vsvrsup_cs(VSvrID) || VSvrID <- node:get_vsvrids()],
  % BattlecheckSup = battle_check_pool_specs(),
  % CacheChildren  = cache_cli:child_specs(),
  lists:flatten([RedisSup, EhttpcSup, LruSup]).
  % lists:flatten([Monitor, SlowDBSup, BattlecheckSup, MailSup, RedisSup, VSvrSups, CacheChildren, EhttpcSup, LruSup]).

% battle_check_pool_specs() ->
%  {ok, Pools} = application:get_env(game_svr, battle_check_pools),
%  PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
%     PoolArgs = [{name, {local, Name}},
%                 {worker_module, battle_check}] ++ SizeArgs,
%     poolboy:child_spec(Name, PoolArgs, WorkerArgs)
%  end, Pools),
%  PoolSpecs.

% vsvrsup_cs(VSvrID) ->
%   Name = ?V_MOD(sup_vsvr, VSvrID),
%   {Name, {sup_vsvr, start_link, [Name, VSvrID]}, permanent, 5000, worker, [sup_vsvr]}.