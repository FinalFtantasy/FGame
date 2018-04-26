-ifndef(LIB_H).
-define(LIB_H, lib_h).

%% LIB Settings
-define(DEBUG(Fmt, Args),   lager:debug(Fmt, Args)).
-define(INFO(Fmt, Args),    lager:info(Fmt, Args)).
-define(WARNING(Fmt, Args), lager:warning(Fmt, Args)).
-define(ERROR(Fmt, Args),   lager:error(Fmt, Args)).
-define(OSS(Fmt, Args),			oss:info(Fmt, Args)).

-define(DEBUG(Arg),         lager:debug("~p",   [Arg])).
-define(INFO(Arg),          lager:info("~p",    [Arg])).
-define(WARNING(Arg),       lager:warning("~p", [Arg])).
-define(ERROR(Arg),         lager:error("~p",   [Arg])).
-define(OSS(Arg),		 				oss:info("~p",  [Arg])).

-define(PR(Content), 				lager:pr(Content, ?MODULE, [compress])).
-define(PR_ST(Content), 		lager:pr_stacktrace(erlang:get_stacktrace(), Content)).
-define(PR_ST(Stacktrace, Content), lager:pr_stacktrace(Stacktrace, Content)).

-define(NOW,                upd:now()).

-define(TRUE, true).
-define(FALSE, false).
-define(NOT_FOUND, not_found).

%% DB Settings
-define(GAME_DB_POOL, game_db_pool).

-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CHILD(N, I, Args, Type), {N, {I, start_link, Args}, permanent, 5000, Type, [I]}).


-define(V_CHILD(Mod, Name), {Name, {Mod, start_link, [Name]}, permanent, 5000, worker, [Mod]}).

-define(SVR_INI_SUCC(SvrName), lager:info("svr ~p init successed, node is ~p", [SvrName, node()])).

-define(MAX_LINEUP_CNT, 5).

%% time
-define(SPAN_POINT, 0).  %% 每天0点刷新
-define(ONE_MINUTE_SEC, 60).
-define(ONE_HOUR_SEC, (60 * 60)).
-define(ONE_DAY_SEC, (24 * 60 * 60)).

% test config
-define(NOW_BY_TEST, now_by_test).

-define(V_MOD(Mod, VSvrID), list_to_atom(lists:concat([Mod, "_", VSvrID]))).

-define(IF(BOOL, V1, V2), tools:if_then(BOOL, V1, V2)).

% gmt settings
-define(EQUIP_ID_ADD, 10000).

% oss setings
-define(OSS_DIR, "/log/oss/").

-define(GAME_SVR_CACHE, game_svr_cache).

-define(EQUIP_ATTR_HERO_TAG, "hero_tag").

% 英雄被动加锁类型
-define(LOCAL_ARENA_DEFENSE, local_arena_defense).

-endif.