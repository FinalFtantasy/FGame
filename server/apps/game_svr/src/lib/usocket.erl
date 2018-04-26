%%%-------------------------------------------------------------------
%%% @contrib gerald.xv
%%% @copyright (C) Lilith Games
%%% @doc
%%% socket 工具类
%%% 1. 记录玩家socket用来主动推消息
%%% 2. 用来比较up协议和缓存down协议
%%% @end
%%%-------------------------------------------------------------------
-module(usocket).

-include("up_pb.hrl").
-include("down_pb.hrl").
-include("record.hrl").
-include("ets.hrl").
-include("com_def.hrl").

-export([init_ets/0, fetch_cache_msg/3, update_up_socket/3, update_down_socket/4, send_pb_msg/2,
  update_heartbeat/1, get_last_hb_time/1]).
%%%===================================================================
%%% API
%%%===================================================================
init_ets() ->
  ets:new(?USOCKET_ETS, [set, public, named_table, compressed, {keypos, #usocket.uid}]).
%%--------------------------------------------------------------------
fetch_cache_msg(UID, _Pid, UpMsg) when UID =:= undefined; UID =:= 0; UpMsg#up_msg.repeat =:= 0 ->
  fail;
fetch_cache_msg(UID, _Pid, UpMsg) ->
  PrevMsg = UpMsg#up_msg{repeat = 0},
  case ets:lookup(?USOCKET_ETS, UID) of
    [USocket] when USocket#usocket.up_msg =:= PrevMsg ->
      {ok, USocket#usocket.down_msg};
    _ ->
      fail
  end.
%%--------------------------------------------------------------------
update_up_socket(UID, Pid, UpMsg) ->
  set_usocket(UID, Pid, UpMsg, undefined).
%%--------------------------------------------------------------------
update_down_socket(UID, Pid, UpMsg, DownMsg) ->
  set_usocket(UID, Pid, UpMsg, DownMsg).
%%--------------------------------------------------------------------
update_heartbeat(UID) ->
  NewUsocket =
    case ets:lookup(?USOCKET_ETS, UID) of
      [UScoket] ->
        UScoket#usocket{last_hb_time = ?NOW};
      [] ->
        #usocket{uid = UID, last_hb_time = ?NOW}
    end,
  ets:insert(?USOCKET_ETS,NewUsocket),
  update_cache_hb_time(UID).
%%--------------------------------------------------------------------
get_last_hb_time(UID) ->
  case ets:lookup(?USOCKET_ETS, UID) of
    [UScoket] ->
      UScoket#usocket.last_hb_time;
    [] ->
      undefined
  end.
%%--------------------------------------------------------------------
get_cache_hb_time(UID) ->
  case cache_cli:get(?GAME_SVR_CACHE, {last_hb_time, UID}) of
    {ok, LastHBTime} ->
      LastHBTime;
    _ ->
      0
  end.
%%--------------------------------------------------------------------
update_cache_hb_time(UID) ->
  cache_cli:set(?GAME_SVR_CACHE, {last_hb_time,  UID}, ?NOW).
%%--------------------------------------------------------------------
send_pb_msg(UID, PbMsg) when is_record(PbMsg, down_msg) ->
  case ets:lookup(?USOCKET_ETS, UID)  of
    [#usocket{pid = Pid}]->
      case catch erlang:is_process_alive(Pid) of
        true ->
          NewPbMsg = PbMsg#down_msg{reply_svr_ts = time_util:timenow()},
          EnPbMsg  = down_pb:encode_msg(NewPbMsg),
          Pid!{pb_msg, EnPbMsg};
        _ ->
          ok
      end;
    _ ->
      ok
  end;
send_pb_msg(_UID, _) ->
  ok.
%%%===================================================================
%%% Internal
%%%===================================================================
set_usocket(undefined, _Pid, _UpMsg, _DownMsg) ->
  ok;
set_usocket(UID, Pid, UpMsg, DownMsg) ->
  USocket = #usocket{uid = UID, pid = Pid, up_msg = UpMsg, down_msg = DownMsg},
  ets:insert(?USOCKET_ETS, USocket).