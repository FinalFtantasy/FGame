-module(redis).

%% Default timeout for calls to the client gen_server
%% Specified in http://www.erlang.org/doc/man/gen_server.html#call-3
-define(TIMEOUT, 5000).

-include("com_def.hrl").
-include("err_code.hrl").

-export([q/2, q/3, qp/2, qp/3, transaction/2]).

%%--------------------------------------------------------------------
%% @doc
%% Executes the given command in the specified connection. The
%% command must be a valid Redis command and may contain arbitrary
%% data which will be converted to binaries. The returned values will
%% always be binaries.
%% @end
%%--------------------------------------------------------------------
-spec q(PoolName::atom(), Command::iolist()) ->
  {ok, binary() | [binary()]} | {error, Reason::binary()}.

q(PoolName, Command) ->
  q(PoolName, Command, ?TIMEOUT).

-spec q(PoolName::atom(), Command::iolist(), Timeout::integer()) ->
  {ok, binary() | [binary()]} | {error, Reason::binary()}.

q(PoolName, Command, Timeout) ->
  case poolboy:transaction(PoolName, fun(Worker) -> eredis:q(Worker, Command, Timeout) end) of
    {ok, Value} ->
      {ok, Value};
    {error, Reason} ->
      ?ERROR("[redis] q error, reason: ~p", [Reason]),
      ?ERR_REDIS_ISSUE
  end.

-spec qp(PoolName::atom(), Command::iolist(), Timeout::integer()) ->
  {ok, binary() | [binary()]} | {error, Reason::binary()}.

qp(PoolName, Pipeline) ->
  qp(PoolName, Pipeline, ?TIMEOUT).

qp(PoolName, Pipeline, Timeout) ->
  poolboy:transaction(PoolName, fun(Worker) -> eredis:qp(Worker, Pipeline, Timeout) end).

transaction(PoolName, Fun) when is_function(Fun) ->
  F = fun(C) ->
    try
      {ok, <<"OK">>} = eredis:q(C, ["MULTI"]),
      Fun(C),
      eredis:q(C, ["EXEC"])
    catch Klass:Reason ->
      {ok, <<"OK">>} = eredis:q(C, ["DISCARD"]),
      io:format("Error in redis transaction. ~p:~p",
        [Klass, Reason]),
      {Klass, Reason}
    end
      end,
  poolboy:transaction(PoolName, F).