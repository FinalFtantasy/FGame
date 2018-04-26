-module(ehttpc_worker).

-behaviour(gen_server).

-include("com_def.hrl").
-include("err_code.hrl").

%% API functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {address, profile}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    Address = proplists:get_value(address, Args),
    Profile = proplists:get_value(profile, Args),
    {ok, #state{address = Address, profile = Profile}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get, UrlParam}, _From, State = #state{profile = Profile}) ->
    Url = get_url(State, UrlParam),
    Result = ehttpc_worker:get(Url, Profile),
    {reply, Result, State};
handle_call({post, UrlParam, ContentType, Body}, _From, State = #state{profile = Profile}) ->
    Url = get_url(State, UrlParam),
    Result = ehttpc_worker:post(Url, ContentType, Body, Profile),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get(Url, Profile) ->
  ?DEBUG("url: ~p", [Url]),
  Res = httpc:request(get, {Url, []}, [{timeout, 6000}], [], Profile),
  parse_res(Res).

post(Url, ContentType, Body, Profile) ->
  Res = httpc:request(post, {Url, [], ContentType, Body}, [{timeout, 6000}], [], Profile),
  parse_res(Res).

parse_res(Res) ->
  case Res of
    {ok, Result} ->
      {ok, Result};
    {error, Error} ->
      ?WARNING("ehttpc request error ~p, res: ~p", [Error, Res]),
      ?ERR_HTTP_REQUEST
  end.

get_url(#state{address = Address}, UrlParam) ->
  "http://" ++ Address ++ UrlParam. 
