-module(redis_cli).

-define(REDIS_POOL, redis_pool).

-export([set/2, get/1, mset/1, mget/1, mget/2, incr/1]).
-export([sadd/2, smembers/1]).
-export([hget/2, hset/3, hgetall/1, hdel/2]).
-export([zrem/2, zadd/3, zadd/2, zrevrange/3, zrevrange/4, zrevrank/2, zscore/2, zincrby/3, hmget/2]).
-export([hscan/3, hscan/4]).
-export([lrange/3, llen/1, lpush/2, ltrim/3]).
-export([cmd/1, eval/3]).
-export([delete/1]).
-export([get_int/1, get_str/1, get_int/2, get_str/2]).
%% =============================================================================
%% Hash
%% =============================================================================
hset(Key, Field, Value) ->
  redis:q(?REDIS_POOL, ["HSET", Key, Field, Value]).

hget(Key, Field) ->
  redis:q(?REDIS_POOL, ["HGET", Key, Field]).

hscan(Key, Cursor, Pattern) ->
  redis:q(?REDIS_POOL, ["HSCAN", Key, Cursor, "MATCH", Pattern]).

hscan(Key, Cursor, Pattern, Count) ->
  redis:q(?REDIS_POOL, ["HSCAN", Key, Cursor, "MATCH", Pattern, "COUNT", Count]).

hmget(_Key, []) ->
  {ok, []};
hmget(Key, FieldList) ->
  redis:q(?REDIS_POOL, ["HMGET", Key|FieldList]).

hgetall(Key) ->
  redis:q(?REDIS_POOL, ["HGETALL", Key]).

hdel(Key, Field) ->
  redis:q(?REDIS_POOL, ["HDEL", Key, Field]).

hincrby(Key, Field, Inc) ->
  redis:q(?REDIS_POOL, ["HINCRBY", Key, Field, Inc]).

set(Key, Value) ->
  case redis:q(?REDIS_POOL, ["SET", Key, Value]) of
    {ok, <<"OK">>} ->
      ok;
    Err ->
      Err
  end.

get(Key) ->
  redis:q(?REDIS_POOL, ["GET", Key]).

mset(KeyValuePairs) ->
  FlattenKeyValuePairs = lists:flatten([[Key, Value] || {Key, Value} <- KeyValuePairs]),
  case redis:q(?REDIS_POOL, ["MSET" | FlattenKeyValuePairs]) of
    {ok, <<"OK">>} ->
      ok;
    Other ->
      Other
  end.

mget(KeyList) ->
  mget(KeyList, []).
mget(KeyList, Default) ->
  case redis:q(?REDIS_POOL, ["MGET" | [Key || Key <- KeyList]]) of
    {ok, ValueList} ->
      [Value || Value <- ValueList];
    _ ->
      Default
  end.

incr(Key) ->
  case redis:q(?REDIS_POOL, ["INCR", Key]) of
    {ok, BinValue} ->
      {ok, erlang:binary_to_integer(BinValue)};
    Other ->
      Other
  end.

%% =============================================================================
%% SET 集合
%% =============================================================================
sadd(Key, Value) ->
  case redis:q(?REDIS_POOL, ["SADD", Key, Value]) of
    {ok, <<"OK">>} ->
      ok;
    Other ->
      Other
  end.

smembers(Key) ->
  case redis:q(?REDIS_POOL, ["SMEMBERS", Key]) of
    {ok, BinValue} ->
      {ok, BinValue};
    Other ->
      Other
  end.
%% =============================================================================
%% ZSET 有序集合
%% =============================================================================
zadd(Key, Score, Value) ->
  case redis:q(?REDIS_POOL, ["ZADD", Key, Score, Value]) of
    {ok, Ret} ->
      {ok, Ret};
    Other ->
      Other
  end.

zadd(Key, List) ->
  redis:q(?REDIS_POOL, lists:append(["ZADD", Key], List)).


zrem(Key, Value) ->
  case redis:q(?REDIS_POOL, ["ZREM", Key, Value]) of
    {ok, Ret} ->
      {ok, Ret};
    Other ->
      Other
  end.

zrevrange(Key, Beg, End) ->
  case redis:q(?REDIS_POOL, ["ZREVRANGE", Key, Beg, End]) of
    {ok, []} ->
      {ok, []};
    {ok, BinValue} ->
      {ok, BinValue};
    Other ->
      Other
  end.

zrevrange(Key, Beg, End, Opt) ->
  case redis:q(?REDIS_POOL, ["ZREVRANGE", Key, Beg, End, Opt]) of
    {ok, []} ->
      {ok, []};
    {ok, BinValue} ->
      {ok, BinValue};
    Other ->
      Other
  end.

zrevrank(Key, Value) ->
  case redis:q(?REDIS_POOL, ["ZREVRANK", Key, Value]) of
    {ok, undefined} -> %% 待测试
      {ok, undefined};
    {ok, BinValue} ->
      {ok, erlang:binary_to_integer(BinValue)};
    Other ->
      Other
  end.

zscore(Key, Value) ->
  case redis:q(?REDIS_POOL, ["ZSCORE", Key, Value]) of
    {ok, undefined} ->
      {ok, undefined};
    {ok, BinValue} ->
      {ok, erlang:binary_to_integer(BinValue)};
    Other ->
      Other
  end.  

zincrby(Key, Increment, Member) ->
  redis:q(?REDIS_POOL, ["ZINCRBY", Key, Increment, Member]).

%% =============================================================================
%% LIST 列表
%% =============================================================================
lrange(Key, Start, Stop) ->
  redis:q(?REDIS_POOL, ["LRANGE", Key, Start, Stop]).

lpush(Key, Value) ->
  redis:q(?REDIS_POOL, ["LPUSH", Key, Value]).

llen(Key) ->
  redis:q(?REDIS_POOL, ["LLEN", Key]).

ltrim(Key, Start, Stop) ->
  case redis:q(?REDIS_POOL, ["LTRIM", Key, Start, Stop]) of
    {ok, <<"OK">>} ->
      ok;
    Other ->
      Other
  end.

cmd(Params) ->
  redis:q(?REDIS_POOL, Params).
%% =============================================================================
%% Hash
%% =============================================================================
%% 只支持keys算长度
eval(Script, Keys, Args) ->
  EArgs = ["EVAL", Script, erlang:length(Keys)] ++ Keys ++ Args,
  redis:q(?REDIS_POOL, EArgs).

get_int(Key, Field) ->
  case hget(Key, Field) of
    {ok, undefined} ->
      {ok, undefined};
    {ok, BinValue} ->
      {ok, binary_to_integer(BinValue)};
    Err ->
      Err
  end.

get_str(Key, Field) ->
  case hget(Key, Field) of
    {ok, undefined} ->
      {ok, undefined};
    {ok, BinValue} ->
      {ok, binary_to_list(BinValue)};
    Err ->
      Err
  end.

get_int(Key) ->
  case redis_cli:get(Key) of
    {ok, undefined} ->
      {ok, undefined};
    {ok, BinValue} ->
      {ok, binary_to_integer(BinValue)};
    Err ->
      Err
  end.

get_str(Key) ->
  case redis_cli:get(Key) of
    {ok, undefined} ->
      {ok, undefined};
    {ok, BinValue} ->
      {ok, binary_to_list(BinValue)};
    Err ->
      Err
  end.
%% =============================================================================
% 删除
delete(Key) ->
  redis:q(?REDIS_POOL, ["DEL", Key]).