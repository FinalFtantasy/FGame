-module(game_handler).

-export([init/2]).

-export([pack_res/1, exec_cmd_proc/2]).

-include("settings.hrl").

-import(tools, [proplists_get_from_bin/2, list_json_format/1, json_list_format/1, md5str/1]).

% ---------------------------------------------------------------------
init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Res = do_handler(Method, HasBody, Req0),
  {ok, Res, Opts}.  

% ---------------------------------------------------------------------
do_handler(<<"POST">>, true, Req0) ->
  {ok, ReqBin, Req} = cowboy_req:read_body(Req0),
  ?DEBUG("post true : reqbin: ~p~nreq:~p", [ReqBin, Req]),
  ReqData = binary_to_list(ReqBin),
  ReqProp = req_data_format(ReqData),
  ?DEBUG("req prop: ~p", [ReqProp]),
  ResMsg  = exec_cmd(ReqProp),
  ?DEBUG("gm res msg: ~p", [ResMsg]),
  ResJson = pack_res(ResMsg),
  ?DEBUG("gm res json: ~p", [ResJson]),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"text/plain; charset=utf-8">>
  }, ResJson, Req);
do_handler(<<"POST">>, false, Req) ->
  ?DEBUG("post false :~p", [Req]),
  cowboy_req:reply(400, #{}, <<"Missing body.">>, Req);
do_handler(_, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(400, Req).

% ---------------------------------------------------------------------
req_data_format(ReqData) ->
  % ReqMsg  = base64:decode(ReqData),
  ReqJson = jiffy:decode(ReqData),
  ReqProp = json_list_format(ReqJson),
  ReqProp.

exec_cmd(ReqMsg) ->
  Cmd     = proplists:get_value("cmd", ReqMsg),
  ResMsg  = exec_cmd_proc(Cmd, ReqMsg),
  ResMsg.

% ---------------------------------------------------------------------
exec_cmd_proc("room_in", ReqMsg) ->
  Res = room:room_in(ReqMsg),
  Res;
exec_cmd_proc("room_exit", ReqMsg) ->
  Res = room:room_exit(ReqMsg),
  Res;
exec_cmd_proc("state", ReqMsg) ->
  Res = player:state(ReqMsg),
  Res;
exec_cmd_proc("watch_you", ReqMsg) ->
  Res = player:watch_you(ReqMsg),
  Res;
exec_cmd_proc("catch_you", ReqMsg) ->
  Res = player:catch_you(ReqMsg),
  Res;
exec_cmd_proc(_Cmd, _) ->
  Res = [{ret, -1}, {ret_info, list_to_binary("unknown_req")}],
  Res.

% ===========================================================
pack_res(Res) ->
  ResFormat = list_json_format(Res),
  ?DEBUG("res format :~p", [ResFormat]),
  jiffy:encode(ResFormat).