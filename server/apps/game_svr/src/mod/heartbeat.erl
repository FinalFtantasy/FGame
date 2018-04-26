-module(heartbeat).

-export([req/2]).

-include("up_pb.hrl").
-include("down_pb.hrl").`

%%% ================================ export ================================
req(UID, #req_heartbeat{time = Req}) when Req =/= undefined  ->
  usocket:update_heart_beat(UID),
  #reply_heartbeat{delay = 5};
req(_UID, _Req) ->
	?ERR_UNEXPECTED_REQUEST.