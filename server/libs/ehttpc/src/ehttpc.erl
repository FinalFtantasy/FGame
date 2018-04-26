-module(ehttpc).

-export([request/2, notify_json/3]).
-export([notify_json_i/3]).

request(PoolName, UrlParam) -> 
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:call(Worker, {get, UrlParam})
  end).

notify_json(PoolName, UrlParam, Data) ->
  spawn_link(?MODULE, notify_json_i, [PoolName, UrlParam, Data]).

notify_json_i(PoolName, UrlParam, Data) ->
  poolboy:transaction(PoolName, fun(Worker) ->
      gen_server:call(Worker, {post, UrlParam, "application/json", Data})
  end).



