-module(csv_parser).
-author("ltb<lintingbin31@gmail.com>").

-export([parse/1]).

-include("record.hrl").

-define(CSV_COMMA, ",").
-define(CSV_LINE_END, "\r\n|\n|\r").
-define(ARRAY_SEMICOLON, ";").

parse(File) ->
  put(file, File),
  case file:read_file(File) of
    {ok, BinData} ->
      Data = binary_to_list(BinData),
      AllLines = re:split(Data, ?CSV_LINE_END, [{return, list}, trim]),
      {Attrs, Lines} =
        case AllLines of
          [Types, Names| Tail] ->
            TypeList = re:split(Types, ?CSV_COMMA, [{return, list}]),
            NameList = re:split(Names, ?CSV_COMMA, [{return, list}]),
            {build_attr(NameList, TypeList), Tail};
          _ ->
            error_exit(type_and_name_undefined)
        end,
      ErlData = parse_lines(Lines, Attrs, []),
      {ErlData, Attrs};
    {error, Reason} ->
      error_exit(Reason)
  end.

build_attr(NameList, TypeList) ->
  build_attr(NameList, TypeList, [], 1).

build_attr([], [], Attrs, _) ->
  lists:reverse(Attrs);
build_attr([Name| Ntail], [Types| Ttail], Attrs, Column) ->
  NewColumn = Column + 1,
  Attr = build_column_attr(Types),
  case Attr#column_attr.type =:= undefined of
    true ->
      build_attr(Ntail, Ttail, Attrs, NewColumn);
    false ->
      AtomName = format_atom_name(Name),
      NewAttr = Attr#column_attr{col = Column, name = AtomName},
      build_attr(Ntail, Ttail, [NewAttr| Attrs], NewColumn)
  end.

format_atom_name(Str) ->
  Str1  = conti_upper_to_camel(Str, false, []),
  Str2  = string:join(string:tokens(Str1, "_"), "_"),
  Str3  = string:to_lower(Str2),
  Str4  = list_to_atom(Str3),
  Str4.

% 特殊大写情况
conti_upper_to_camel([], _, Str) ->
  lists:reverse(Str);
conti_upper_to_camel([$U, $I | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$i, $U, $_ | Str]);
conti_upper_to_camel([$I, $D | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$d, $I, $_ | Str]);
conti_upper_to_camel([$C, $D | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$d, $C, $_ | Str]);
conti_upper_to_camel([$G, $S | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$s, $G, $_ | Str]);
conti_upper_to_camel([$P, $V, $P | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$p, $v, $P, $_ | Str]);
conti_upper_to_camel([$P, $V, $E | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$e, $v, $P, $_ | Str]);
conti_upper_to_camel([$T, $I, $D | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$d, $i, $T, $_ | Str]);
conti_upper_to_camel([$V, $I, $P | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$p, $i, $V, $_ | Str]);
% 特殊字符处理
conti_upper_to_camel([$+ | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, "_dda" ++ Str);
conti_upper_to_camel([32 | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, [$_ | Str]);
conti_upper_to_camel([$% | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, "_rep" ++ Str);
conti_upper_to_camel([$. | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, "_tod" ++ Str);
conti_upper_to_camel([$( | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, Str);
conti_upper_to_camel([$) | Left], _IsPrevUp, Str) ->
  conti_upper_to_camel(Left, false, Str);
% 大小写和数字处理
conti_upper_to_camel([C | Left], IsPrevUp, Str) ->
  case is_upper(C) of
    true when IsPrevUp ->
      conti_upper_to_camel(Left, true, [C + 32 | Str]);
    true ->  
      conti_upper_to_camel(Left, true, [C, $_ | Str]);
    false ->
      case is_number_char(C) of
        true  ->
          conti_upper_to_camel(Left, false, [C, $_ | Str]);
        false ->
          conti_upper_to_camel(Left, false, [C | Str])
      end
  end.

is_upper(C) ->
  (is_integer(C) andalso $A =< C andalso C =< $Z) orelse
    (is_integer(C) andalso 16#C0 =< C andalso C =< 16#D6) orelse
    (is_integer(C) andalso 16#D8 =< C andalso C =< 16#DE).

is_number_char(C) ->
  (is_integer(C) andalso $0 =< C andalso C =< $9).

build_column_attr(Attrs) ->
  build_column_attr(Attrs, #column_attr{}).

build_column_attr([], Attr) -> Attr;
build_column_attr([$O| _], Attr) -> Attr;
build_column_attr([$A, $S, $R | _Tail], Attr) ->
  Attr#column_attr{is_array = true, type = {asset, undefined}};
build_column_attr([$K| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_key = true});
build_column_attr([$I| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_index = true});
build_column_attr([$A| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{is_array = true});
build_column_attr([$S, $T| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {atom, undefined}});
build_column_attr([$T, $S| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {atom, [$", $"]}});
build_column_attr([$N| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {integer, 0}});
build_column_attr([$S| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {string, [$", $"]}});
build_column_attr([$B| Tail], Attr) ->
  build_column_attr(Tail, Attr#column_attr{type = {bool, false}});
build_column_attr([_| Tail], Attr) ->
  build_column_attr(Tail, Attr).

parse_lines([], _, Res) -> 
  lists:reverse(Res);
parse_lines([Line| Tail], Attrs, Res) ->
  List = line_to_list(Line, []),
  Data = parse_line(List, Attrs),
  parse_lines(Tail, Attrs, [Data| Res]).

line_to_list(last_is_empty, Done) ->
  lists:reverse([[]| Done]);
line_to_list([], Done) -> 
  lists:reverse(Done);
line_to_list([$"| Tail], Done) ->
  {One, Tail1} = get_field_from_list(Tail, [], true),
  line_to_list(Tail1, [One| Done]);
line_to_list(List, Done) ->
  {One, Tail1} = get_field_from_list(List, [], false),
  line_to_list(Tail1, [One| Done]).

get_field_from_list([$,], Done, _IsOne) ->
  {lists:reverse(Done), last_is_empty};
get_field_from_list([], Done, _IsOne) ->
  {lists:reverse(Done), []};
get_field_from_list([$",$,| Tail], Done, _IsOne) ->
  {lists:reverse(Done), Tail};
get_field_from_list([$,| Tail], Done, true) ->
  get_field_from_list(Tail, [$,| Done], true);
get_field_from_list([$,| Tail], Done, false) ->
  {lists:reverse(Done), Tail};
get_field_from_list([C| Tail], Done, IsOne) -> 
  get_field_from_list(Tail, [C| Done], IsOne).

parse_line(List, Attrs) ->
  parse_line(List, Attrs, [], 1).

parse_line(List, Attrs, Done, _) when List =:= []; Attrs =:= [] ->
  lists:reverse(Done);
parse_line([Element| Ltail], [#column_attr{col = Col} = Attr| Atail], Done, Col) ->
  ErlData = parse_elment(Element, Attr),
  parse_line(Ltail, Atail, [ErlData| Done], Col + 1);
parse_line([_| Ltail], Atail, Done, Col) ->
  parse_line(Ltail, Atail, Done, Col + 1).

parse_elment(Element, #column_attr{is_array = IsArray, type = Type}) ->
  case IsArray of
    true when Element =:= [] ->
      [];
    true ->
      trans2erlang_array(Type, Element);
    false ->
      trans2erlang_type(Type, Element)
  end.

trans2erlang_array({asset, _Default}, Element) ->
  ElementList = re:split(Element, ?ARRAY_SEMICOLON, [{return, list}]),
  trans2asset_array(ElementList, []);
trans2erlang_array(Type, Element) ->
  ElementList = re:split(Element, ?ARRAY_SEMICOLON, [{return, list}]),
  [trans2erlang_type(Type, X) || X <- ElementList].

trans2asset_array([], Rslt) ->
  lists:reverse(Rslt);
trans2asset_array([Type, ID, N | Left], Rslt) ->
  Asst  = {asset, trans2erlang_atom(Type), trans2erlang_atom(ID), trans2erlang_atom(N)},
  trans2asset_array(Left, [Asst | Rslt]).

trans2erlang_type({_ErlType, Default}, []) ->
  Default;
trans2erlang_type({string, _Default}, Data) ->
  [$"| Data] ++ [$"];
trans2erlang_type({atom, _Default}, Data) ->
  trans2erlang_atom(Data);
trans2erlang_type({integer, _Default}, Data) ->
  Data;
trans2erlang_type({bool, Default}, BoolStr) -> 
  Bool = list_to_atom(string:lowercase(BoolStr)),
  case Bool =:= false orelse Bool =:= true of
    true ->
      Bool;
    false ->
      error_exit({unexcept_type, {bool, Default}, BoolStr})
  end;
trans2erlang_type(ErlType, Unkonw) ->
  error_exit({unexcept_type, ErlType, Unkonw}).

% trans2erlang_integer(Data) ->
  % try 
  %   list_to_integer(Data)
  % catch _:_ ->
  %   try 
  %     list_to_float(Data)
  %   catch _:_ ->
  %     error_exit({error_integer, Data})
  %   end
  % end.

%% 目前只支持string类型
%% 留下拓展接口,目前不支持
trans2erlang_atom(String) ->
  case get_trans_mod() of
    {module, csv_trans} ->
      csv_trans:type(String);
    {error,nofile} ->
      type(String)
  end.

get_trans_mod() ->
  case get(csv_trans) of
    undefined ->
      Rslt = code:ensure_loaded(csv_trans),
      put(csv_trans, Rslt),
      Rslt;
    V ->
      V
  end.

type(String) ->
  case is_datatime(String) of
    true ->
      datetime_to_ts(String);
    false ->
      case string:to_integer(String) of
        {error,  _} ->
          string_to_atom(String);
        {Int, _} ->
          Int
      end
  end.

is_datatime(String) ->
  case re:run(String, ".*/.*/.*:.*:.*") of
    {match, _} ->
      true;
    _ ->
      false
  end.

datetime_to_ts([]) -> 0;
datetime_to_ts(DateTimeStr) ->
  [Y, M, D, H, Min, S] = string:tokens(DateTimeStr, ": /"),
  F = fun list_to_integer/1,
  local_time2seconds({{F(Y), F(M), F(D)}, {F(H), F(Min), F(S)}}).

string_to_atom(Str) ->
  Atom  = format_atom_name(Str),
  Atom.

%% 将本地时间转为时间戳
local_time2seconds(DayTime) ->
  [Time] = calendar:local_time_to_universal_time_dst(DayTime),
  calendar:datetime_to_gregorian_seconds(Time) - 62167219200.

error_exit(Error) ->
  FileName = 
    case get(file) of
      undefined -> 
        unknown_file;
      Name ->
        Name
    end,
  exit({FileName, Error}).