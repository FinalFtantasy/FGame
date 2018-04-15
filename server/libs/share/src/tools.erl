-module(tools).

-include("settings.hrl").
-export([svr_port/0]).

-export([list_json_format/1, json_list_format/1, proplists_get_from_bin/2, md5str/1]).
-export([timenow/0]).

-define(TIMEOUT, 1000).

% =====================================================================
% proplist -> 能被jiffy接受的list输入格式
list_json_format([]) ->
	null;
list_json_format(List) when is_list(List) ->
	% io:format("~nlist:~w", [List]),
	V = [list_json_format(One) || One <- List],
	case List =/= [] andalso is_tuple(lists:last(List)) of
		true  ->
			{V};
		false ->
			V
	end;
list_json_format({K, V}) ->
	% io:format("~nKV:~w,~w", [K, V]),
	{list_json_format(K), list_json_format(V)};
list_json_format(V) when is_atom(V) orelse is_integer(V) orelse is_binary(V) ->
	% io:format("~natom:~w", [V]),
	V;
list_json_format(V) ->
	% io:format("~n~w", [V]),
	list_to_binary(V).

% ---------------------------------------------------------------------
% jiffy对json结构进行解码得到的list输出结构 -> proplist
json_list_format({V}) ->
	json_list_format(V);
json_list_format({K, V}) ->
	{json_list_format(K), json_list_format(V)};
json_list_format(List) when is_list(List) ->
	[json_list_format(One) || One <- List];
json_list_format(V) ->
	case is_binary(V) of 
		true  ->
			binary_to_list(V);
		false ->
			V
	end.

% ---------------------------------------------------------------------
proplists_get_from_bin(Key, Tuple) when is_tuple(Tuple) ->
	{List} = Tuple,
	proplists_get_from_bin(Key, List);
proplists_get_from_bin(Key, List) when is_list(List) ->
	KeyBin = case is_atom(Key) of
		true  ->
			Key;
		false ->
			list_to_binary(Key)
	end,
	case proplists:get_value(KeyBin, List, undefined) of
	    undefined ->  undefined;
	    Val       ->  binary_to_list(Val)
	end.

% ---------------------------------------------------------------------
md5str(S) -> 
  Md5_bin = erlang:md5(S), 
  Md5_list = binary_to_list(Md5_bin), 
  lists:flatten(list_to_hex(Md5_list)). 
 
list_to_hex(L) -> 
  lists:map(fun(X) -> int_to_hex(X) end, L). 
 
int_to_hex(N) when N < 256 -> 
  [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 -> $0 + N; 
hex(N) when N >= 10, N < 16 -> $a + (N-10).
% ---------------------------------------------------------------------

svr_port() ->
	?SVR_PORT.

% =====================================================================
timenow() ->
  {A, B, C} = os:timestamp(),
  A * 1000 * 1000 * 1000 + B * 1000 + C div 1000.