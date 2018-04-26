%% http/1.1默认为keep_alive, 但所有请求都会通过一个gen_server发送，并且不会控制连接数量，超过固定数量后会使用短连接， 该模块会为不同需求创建独立的 httpc_manager gen_server,并且可以设置keep_alve 相关参数， 控制短连接数量
-module(ehttpc_sup).

-behaviour(supervisor).

-include("com_def.hrl").

%% API functions
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pools} = application:get_env(ehttpc, pools),
    start_link(Pools).

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools]).
  

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([Pools]) ->
  PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
      MaxSessions        = proplists:get_value(max_sessions, WorkerArgs, 2),
      MaxKeepAlvieLength = proplists:get_value(max_keep_alive_length, WorkerArgs, 5),
      {ok,_}             = inets:start(httpc, [{profile, Name}, {max_sessions, MaxSessions}, {max_keep_alive_length, MaxKeepAlvieLength}]),
      ok                 = httpc:set_options([{max_sessions, MaxSessions}, {max_keep_alive_length, MaxKeepAlvieLength}], Name),
      PoolArgs           = [{name, {local, Name}}, {worker_module, ehttpc_worker}] ++ SizeArgs,
      poolboy:child_spec(Name, PoolArgs, WorkerArgs ++ [{profile, Name}])
  end, Pools),
  {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
  %===================================================================
%%% Internal functions
%%%===================================================================
