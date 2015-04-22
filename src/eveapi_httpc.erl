%%%-------------------------------------------------------------------
%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2014, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2014 by Roman Dayneko <roman.dayneko@gmail.com>
%%%-------------------------------------------------------------------
-module(eveapi_httpc).

-behaviour(gen_server).

-include("include/log.hrl").

%% API
-export([start_link/0]).

-export([r/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, 
        {
          requests_rate,
          requests_left,
          reset_requests_timer,
          failed_requests_rate,
          failed_requests_left,
          reset_failed_requests_timer
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

r({get, URL}) ->
    gen_server:call(?SERVER, {get, URL}).

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
init([]) ->
    process_flag(trap_exit, true),
    ssl:start(),
    inets:start(),

    {MaxRequests, RequestsInterval} = helper:get_env(erleveapi, requests_rate, {30, 1}),
    {MaxFailedRequests, FailedRequestsInterval} = helper:get_env(erleveapi, failed_request_rate, {300, 3 * 60}),
    
    {ok, RequestsResetTimer} = timer:apply_interval(1000 * RequestsInterval, gen_server, cast, [?SERVER, reset_requests_counter]),
    {ok, FailedRequestsResetTimer} = timer:apply_interval(1000 * FailedRequestsInterval, gen_server, cast, [?SERVER, reset_failed_requests_counter]),

    {ok,
     #state{requests_rate = MaxRequests,
            requests_left = MaxRequests,
            reset_requests_timer = RequestsResetTimer,
            failed_requests_rate = MaxFailedRequests,
            failed_requests_left = MaxFailedRequests,
            reset_failed_requests_timer = FailedRequestsResetTimer
           }
    }.

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
handle_call({get, _URL}, _From, #state{requests_left = 0} = State) ->
    {reply, {error, limit_rate_exceed}, State};
handle_call({get, _URL}, _From, #state{failed_requests_left = 0} = State) ->
    {reply, {error, limit_rate_exceed}, State};
handle_call({get, URL}, _From, #state{requests_left = RequestsLeft, failed_requests_left = FailedRequestLeft} = State) ->
    ?DBG("get URL ~p", [URL]),
    case httpc:request(get, {URL, []}, [], [{body_format, string}]) of 
        {ok, {_, _, Body}} ->
            case eveapi_parser:xml_to_plist(Body) of 
                [{error, Reason}] ->
                    {reply, {error, Reason}, State#state{requests_left = RequestsLeft - 1, failed_requests_left = FailedRequestLeft - 1}};
                Data ->
                    {reply, {ok, Data}, State#state{requests_left = RequestsLeft - 1}}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State#state{requests_left = RequestsLeft - 1, failed_requests_left = FailedRequestLeft - 1}}
    end.

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
handle_cast(reset_requests_counter, #state{requests_rate = Rate} = State) ->
    {noreply, State#state{requests_left = Rate}};
handle_cast(reset_failed_requests_counter, #state{failed_requests_rate = Rate} = State) ->
    {noreply, State#state{failed_requests_left = Rate}}.

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
terminate(_Reason, #state{reset_requests_timer = RequestsResetTimer, reset_failed_requests_timer = FailedRequestsResetTimer} = _State) ->
    {ok, cancel} = timer:cancel(RequestsResetTimer),
    {ok, cancel} = timer:cancel(FailedRequestsResetTimer),
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
