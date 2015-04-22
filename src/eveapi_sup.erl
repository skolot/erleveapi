%%%-------------------------------------------------------------------
%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2014, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2014 by Roman Dayneko <roman.dayneko@gmail.com>
%%%-------------------------------------------------------------------
-module(eveapi_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    
    EVEAPI_HTTPC = {eveapi_httpc, {eveapi_httpc, start_link, []}, Restart, Shutdown, Type, [eveapi_httpc]},

    {ok, {SupFlags, [EVEAPI_HTTPC]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
