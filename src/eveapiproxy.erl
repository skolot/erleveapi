%%%-------------------------------------------------------------------
%%% @author Dayneko Roman <me@h0.org.ua>
%%% @copyright (C) 2014, Dayneko Roman
%%% @doc
%%%
%%% @end
%%% Created : 26 Aug 2014 by Dayneko Roman <me@h0.org.ua>
%%%-------------------------------------------------------------------
-module(eveapiproxy).

-behaviour(gen_server).

-include("include/eveapi.hrl").
-include("include/log.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export(
   [
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]
  ).

%% EVE PROXY API
-export(
   [
    start/2,
    r/3,
    set_active_char/2
   ]
  ).


-define(SERVER, ?MODULE).

-record(state,
        {
          baseurl,
          charid,
          keyid,
          vcode,
          requests_count = 0,
          failed_request = 0
        }
       ).

-define(URLKEYMAP,
        #{
          keyid => "keyID",
          vcode => "vCode",
          charid => "characterID",
          ids => "Ids"
         }
       ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(KeyId, VCode) ->
    SName = erlang:list_to_atom(erlang:integer_to_list(KeyId)),
    Spec = {SName, {eveapiproxy, start_link, [SName, KeyId, VCode]}, permanent, 5000, worker, []},
    case supervisor:start_child(eveapi_sup, Spec) of
        {error, _} = Error ->
            {error, Error};
        {ok, _Pid} ->
            {ok, SName}
    end.

start_link(SName, KeyId, VCode) ->
    gen_server:start_link({local, SName}, ?MODULE, [{KeyId, VCode}], []).

r(SName, Request, Args) ->
    gen_server:call(SName, {request, Request, Args}).

set_active_char(SName, CharId) ->
    gen_server:cast(SName, {set_active_char, CharId}).

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
init([{KeyId, VCode}]) ->
    process_flag(trap_exit, true),
    BaseUrl = helper:get_env(eveapi, baseurl, "https://api.eveonline.com/"),
    {ok, #state{baseurl = BaseUrl, keyid = KeyId, vcode = VCode, charid = undefined}}.

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
handle_call({request, {RType, _} = _Request, _Args}, _From, #state{charid = undefined} = State) when RType =/= 'Account' ->
    {reply, {error, active_char_dont_set}, State};
handle_call({request, Request, Args}, _From, #state{} = State) ->
    URL = construct_url(Request, Args, State),
    Reply = eveapi_httpc:r({get, URL}),
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
handle_cast({set_active_char, CharId}, #state{} = State) ->
    {noreply, State#state{charid = CharId}};
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

construct_url(Request, Args, #state{baseurl = BaseUrl} = State) ->
    Args0 = prepare_args(Request, Args),
    Args1 = requests_specific_vars(Request, State),

    ?DBG("Args0 ~p Args1 ~p", [Args0, Args1]),

    ArgsStr =
        string:join(
          lists:map(
            fun({Key, Val}) ->
                    maps:get(Key, ?URLKEYMAP, "none") ++ "=" ++ helper:to_list(Val)
            end,
            Args0 ++ Args1
           ),
          "&"
         ),

    ?DBG("ArgsStr ~p", [ArgsStr]),

    RequestURL = helper:plist_getval(Request, ?REQUESTS, "none"),
    BaseUrl ++ RequestURL ++ "?" ++ ArgsStr.

prepare_args({'EVE', 'TypeName'} = _Request, Args) ->
    Ids0 = helper:plist_getval(ids, Args, []),
    Ids1 = string:join(lists:map(fun(E) -> helper:to_list(E) end, Ids0), ","),
    helper:plist_replace(ids, Ids1, Args);
prepare_args(_Request, Args) ->
    Args.

requests_specific_vars({RType, _}, #state{keyid = KeyId, charid = CharId, vcode = VCode}) when RType == 'Char' orelse RType == 'Corp' ->
    [{keyid, erlang:integer_to_list(KeyId)}, {charid, erlang:integer_to_list(CharId)}, {vcode, erlang:binary_to_list(VCode)}];
requests_specific_vars({'Account', _}, #state{keyid = KeyId, vcode = VCode}) ->
    [{keyid, erlang:integer_to_list(KeyId)}, {vcode, erlang:binary_to_list(VCode)}];
requests_specific_vars(_, #state{}) ->
    [].
