%%%-------------------------------------------------------------------
%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2014, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2014 by Roman Dayneko <roman.dayneko@gmail.com>
%%%-------------------------------------------------------------------
-module(eveapi).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-export(
   [
    start_proxy/2,
    r/2, r/3,
    set_active_char/2
   ]
  ).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start() ->
    start([], []).

start(_StartType, _StartArgs) ->
    case eveapi_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

start_proxy(KeyId, VCode) ->
    case eveapiproxy:start(KeyId, VCode) of
        {ok, SName} ->
            case eveapiproxy:r(SName, {'Account', 'Characters'}, []) of
                {ok, CharactersList} ->
                    {ok, {SName, CharactersList}};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

set_active_char(SName, CharId) ->
    eveapiproxy:set_active_char(SName, CharId).

r(SName, Request) ->
    r(SName, Request, []).

r(SName, Request, Args) ->
    eveapiproxy:r(SName, Request, Args).

%%%===================================================================
%%% Internal functions
%%%===================================================================
