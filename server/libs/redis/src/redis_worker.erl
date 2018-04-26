-module(redis_worker).

-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

start_link(Args) ->
  case eredis:start_link(Args) of
    {ok, Pid} ->
      {ok, Pid};
    {error, Error} ->
      lager:error("start eredis failed, reason: ~p~n", [Error]),
      timer:sleep(1000),
      start_link(Args)
  end.