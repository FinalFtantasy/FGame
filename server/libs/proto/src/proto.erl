-module(proto).

%% API exports
-export([get_enums/2
        , get_fields/2
        , check_enum/3
        , check_field/3
        ]).

-include("err_code.hrl").
-include("gpb.hrl").

%%====================================================================
%% API functions
%%====================================================================
get_enums(Proto, EnumType) ->
	Mod = list_to_atom(lists:concat([Proto, "_pb"])),
	Def = Mod:fetch_enum_def(EnumType),
	[V || {V, _No} <- Def].

get_fields(Proto, MsgType) ->
  Mod = list_to_atom(lists:concat([Proto, "_pb"])),
  Def = Mod:find_msg_def(MsgType),
  [Field#field.name || Field <- Def].

check_enum(Proto, EnumType, Enum) ->
  Enums = get_enums(Proto, EnumType),
  case lists:member(Enum, Enums) of
    true -> ok;
    false -> ?ERR_PROTO_ENUM
  end.

check_field(Proto, MsgType, Field) ->
  Fields = get_fields(Proto, MsgType),
  case lists:member(Field, Fields) of
    true -> ok;
    false -> ?ERR_PROTO_FIELD
  end.