%%%-------------------------------------------------------------------
%% @doc csv_lib public API
%% @end
%%%-------------------------------------------------------------------

-module(csv_lib_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    csv_lib_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
