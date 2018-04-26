-module(user_default).

-include("ets.hrl").
-include("record.hrl").
-include("redis.hrl").
-include("slow_db_settings.hrl").
-include("warthog_push_pb.hrl").
-include("warthog_pack_pb.hrl").
-include("warthog_msg_pb.hrl").
-include("warthog_im_pb.hrl").
-include("warthog_iminner_pb.hrl").
-include("warthog_biz_pb.hrl").
-include("up_pb.hrl").
-include("down_pb.hrl").
-include("common_pb.hrl").
-include("bc_up_pb.hrl").
-include("bc_down_pb.hrl").
-include("game_pb.hrl").
-include("com_def.hrl").
-include("err_code.hrl").
-include("share.hrl").
-include("csv.hrl").

-define(INCLUDE_DIRS, [{i, "libs/csv_lib/include/"},
                        {i, "libs/share/include/"},
                        {i, "libs/proto/include/"},
                        {i, "apps/game_svr/include"}]).
-export([print/1, printf/1]).

-dialyzer({[no_missing_calls], [c/0, cover/1, cover_dump/1]}).

-export([name/1]).

-export([c/0, info_log/0, debug_log/0, error_log/0, cover/1, cover_dump/1]).

print(S)->
  io:format("~w~n",[S]).

printf(S)->
  io:format("~p~n",[S]).

name(UID) ->
  {ok, User} = users:get(UID),
  NC   = User#user.name_card,
  NC#name_card.name.

c() ->
  r3:do(compile).

info_log() ->
  lager:set_loglevel(lager_console_backend, info).

debug_log() ->
  lager:set_loglevel(lager_console_backend, debug).

error_log() ->
  lager:set_loglevel(lager_console_backend, error).

cover(Src) ->
  cover:start(),
  cover:compile(Src, ?INCLUDE_DIRS).

cover_dump(Mod) ->
  cover:analyse_to_file(Mod),
  cover:stop().