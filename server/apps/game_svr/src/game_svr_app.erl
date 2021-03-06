%%%-------------------------------------------------------------------
%% @doc game_svr public API
%% @end
%%%-------------------------------------------------------------------

-module(game_svr_app).

-behaviour(application).

-include("settings.hrl").
%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	Port 	= tools:svr_port(),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", game_handler, []}
    ]}
  ]),
  {ok, _Pid} = cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch}
  }),
  ?INFO("game svr is started"),
  game_svr_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================