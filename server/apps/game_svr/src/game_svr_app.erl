%%%-------------------------------------------------------------------
%% @doc game_svr public API
%% @end
%%%-------------------------------------------------------------------

-module(game_svr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    game_svr_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
