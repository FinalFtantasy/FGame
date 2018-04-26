%%%-------------------------------------------------------------------
%%% @contrib gerald.xv
%%% @copyright (C) Lilith Games
%%% @doc
%%% user process dictionry, reply_extra
%%% @end
%%%-------------------------------------------------------------------
-module(upd).

-include("com_def.hrl").

%% API
-export([erase/0]).

-export([now/0]).

erase() ->
  erlang:erase(now).

now()->
  case erlang:get(now) of
    TN when is_integer(TN) ->
      TN;
    undefined ->
      TN = time_util:timenow(),
      erlang:put(now, TN),
      TN
  end.