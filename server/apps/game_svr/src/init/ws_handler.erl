-module(ws_handler).

-export([start_handler/0, init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-include("up_pb.hrl").
-include("down_pb.hrl").
-include("com_def.hrl").
-include("err_code.hrl").

-export([exec_cmd/1, pack_down_msg/2]).

start_handler() ->
  WsPort   = config:get_int(game_svr, gs_port),
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", ws_handler, []}
    ]}
  ]),
  {ok, Pid} = cowboy:start_clear(http, [{port, WsPort}], #{
    env => #{dispatch => Dispatch}
  }),
  {ok, Pid}.
init(Req, State) ->
  Opts = #{idle_timeout  => infinity},
  {cowboy_websocket, Req, State, Opts}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    ?DEBUG("self ~p, text msg ~p", [self(), Msg]),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle({binary, UpData}, State) ->
    UpMsg = up_pb:decode_msg(UpData, up_msg),
    % reply_extra:f(),
    upd:erase(),
    case exec_cmd(UpMsg) of 
      DownMsg when is_record(DownMsg, down_msg) ->
        Reply = down_pb:encode_msg(DownMsg),
        {reply, {binary, Reply}, State};
      _Err ->
        {ok, State}
    end;
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({pb_msg, PbMsg}, State) ->
  ?DEBUG("send pb msg: ~p ~p", [self(), PbMsg]),
  {reply, {binary, PbMsg}, State};
websocket_info({timeout, _Ref, Msg}, State) ->
  {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
  {ok, State}.

exec_cmd(UpMsg) when UpMsg#up_msg.req_heartbeat =/= undefined  ->
  Reply = heartbeat:req(UpMsg#up_msg.uid, UpMsg#up_msg.req_heartbeat),
  pack_down_msg(UpMsg#up_msg.seq, Reply);
exec_cmd(UpMsg) ->
  ?DEBUG("up msg: ~p", [?PR(UpMsg)]),
  [up_msg, UID, Seq, _Repeat| Cmds] = tuple_to_list(UpMsg),
  Pid = self(),
  usocket:update_heartbeat(UID),
  case usocket:fetch_cache_msg(UID, Pid, UpMsg) of
    {ok, CacheDownMsg} when is_record(CacheDownMsg, down_msg)->
      DownMsg = pack_down_msg(Seq, CacheDownMsg);
    {ok, _CacheDownMsg} ->
      DownMsg = pack_down_msg(Seq, no_reply); %% 与客户端约定no_reply错误码有特别意义，不要改动
    fail ->
      usocket:update_up_socket(UID, Pid, UpMsg),
      Reply   = do_exec_cmd(UID, Seq, Cmds), %% 这句话有可能会崩溃，所以要两个地方都加上update socket
      DownMsg = pack_down_msg(Seq, Reply),
      usocket:update_down_socket(UID, Pid, UpMsg, DownMsg),
      DownMsg
  end,
  ?DEBUG("down msg: ~p", [?PR(DownMsg)]),
  DownMsg.

do_exec_cmd(UID, _Seq, Cmds) ->
  case fetch_exec_cmd(Cmds, 5) of %% 5是给日志用的
    {Cmd, Pos} ->
      User  = users:get_or_create(UID),
      % User2 = intl_mail:read(User),
      % User3 = users:hourly_update(User2),
      % User4 = users:daily_update(User3),
      % User5 = users:weekly_update(User4),
      UserF = User,
      Reply = ws_dispatch:exec_cmd(UserF, Cmd, Pos),
      Reply;
    fail ->
      ?ERR_UNEXPECTED_REQUEST
  end.


fetch_exec_cmd([], _) ->
  fail;
fetch_exec_cmd([undefined| Tail], Pos) ->
  fetch_exec_cmd(Tail, Pos + 1);
fetch_exec_cmd([Cmd| _Tail], Pos) ->
  {Cmd, Pos}.

% 封装下发协议
pack_down_msg(Seq, Msg) ->
%%  ?DEBUG(Msg),
  DownMsg = pack_down_msg_i(Msg),
  DownMsg#down_msg{reply_seq = Seq, reply_svr_ts = ?NOW}.
  % DownMsg#down_msg{reply_seq = Seq, reply_svr_ts = ?NOW, reply_extra = reply_extra:pack()}.

pack_down_msg_i(no_reply) ->
  no_reply;
pack_down_msg_i(Error) when is_atom(Error) ->
  build_err_code(Error);
pack_down_msg_i(Msg) when is_record(Msg, down_msg) ->
  Msg;
pack_down_msg_i(Msg) ->
  MsgType = element(1, Msg),
  Fields  = record_info(fields, down_msg),
  Index   = tools:index_of(MsgType, Fields),
  erlang:setelement(Index + 1, #down_msg{}, Msg).

build_err_code(Error) when is_atom(Error) ->
  ErrorStr = atom_to_list(Error),
  #down_msg{reply_err_code = #reply_err_code{err_code = ErrorStr}}.