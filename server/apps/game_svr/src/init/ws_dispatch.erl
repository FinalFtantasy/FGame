-module(ws_dispatch).

-include("up_pb.hrl").
-include("down_pb.hrl").
-include("err_code.hrl").
-include("com_def.hrl").
-include("game_pb.hrl").

-export([exec_cmd/3]).

exec_cmd(User, Msg, Pos) ->
  Action = get_action_id(Pos, Msg),
  log_svr:create_log_rec(User#user.uid, Action),
  Mod = get_req_mod(Msg),
  dispatch_cmd(User, Mod, Msg).

get_req_mod(Msg) ->
  Cmd    = element(1, Msg),
  CmdStr = atom_to_list(Cmd),
  {"req", [$_| ModStr]} = lists:splitwith(fun(A) -> A =/= $_ end, CmdStr),
  list_to_atom(ModStr).

dispatch_cmd(User, Mod, Msg) ->
  try Mod:req(User, Msg) of
    Result ->
      Result
  catch
    Class:Reason ->
      Stacktrace = erlang:get_stacktrace(), %% catch后马上执行，不然erlang:get_stacktrace()有可能改变
      case fetch_err_code(Reason) of
        {ok, Error} ->
          Error;
        fail ->
          monitor:notify(ws_dispatch_crash, io_lib:format("<error-info: ~p:req ~p:~p>", [Mod, Class, Reason])),
          ?ERROR("Req Msg: ~p.~nStacktrace: ~s", [?PR(Msg), ?PR_ST(Stacktrace, {Class, Reason})]),
          ?ERR_AT_DISPATCH_CMD
      end
  end.

fetch_err_code({badmatch ,{error, Error}}) ->
  {ok, Error};
fetch_err_code({badmatch, Error}) ->
  case tools:is_err_code(Error) of
    true ->
      {ok, Error};
    false ->
      fail
  end;
fetch_err_code(_) ->
  fail.

get_action_id(Pos, Msg) ->
  [_| List] = tuple_to_list(Msg),
  SecPos = get_sec_pos(List, 2),
  Pos * 1000 + SecPos.

get_sec_pos([], _) -> 0;
get_sec_pos([[]| Tail], Pos) ->
  get_sec_pos(Tail, Pos + 1);
get_sec_pos([undefined| Tail], Pos) ->
  get_sec_pos(Tail, Pos + 1);
get_sec_pos(_, Pos) ->
  Pos.
