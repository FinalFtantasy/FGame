-define(SVR_PORT, 30000).
-define(ALLOWED_INTERVAL_TIME, 300).
-define(ROOM_MAX_INDEX, 8).
-define(ROOM_ID_MAX, 999999).
-define(ROOM_ID_MIN, 1000).
-define(INIT_SCORE, 0).
-define(LOCATION_MAX, 100).

% ==========================================
-define(DEBUG(Fmt, Args),    io:format(Fmt ++ "~n", Args)).
-define(INFO(Fmt, Args),     io:format(Fmt ++ "~n", Args)).
-define(DEBUG(Arg),          io:format(Arg ++ "~n")).
-define(INFO(Arg),           io:format(Arg ++ "~n")).

% ==========================================
-define(ROOM_ETS, room_ets).
-define(SCORE_RECORD_ETS, score_record_ets).

% ==========================================
-record(room, {room_id = 0, players = []}).
-record(player, {uid = 0, index = -1, location = 0, speed = 0, score = ?INIT_SCORE, last_update_time = 0}).
% key = {room_id, uid}
-record(score_record, {key, score}).