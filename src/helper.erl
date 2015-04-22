%%%-------------------------------------------------------------------
%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2014, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2014 by Roman Dayneko <roman.dayneko@gmail.com>
%%%-------------------------------------------------------------------
-module(helper).

%% API
-export(
   [
    get_env/2,
    get_env/3,
    plist_getval/2,
    plist_getval/3,
    plist_replace/3,
    to_list/1
   ]
  ).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

get_env(App, Name) ->
    get_env(App, Name, undefined).

get_env(App, Name, Default) ->
    case application:get_env(App, Name) of
        {ok, Value} -> Value;
        _ -> Default
    end.

plist_getval(Key, PList) ->
    plist_getval(Key, PList, undefined).

plist_getval(Key, PList, Default) ->
    case lists:keyfind(Key, 1, PList) of 
        false ->
            Default;
        {Key, Value} ->
            Value
    end.

plist_replace(Key, Val, PList) ->
    lists:keydelete(Key, 1, PList) ++ [{Key, Val}].


to_list(I) when is_integer(I) ->
    erlang:integer_to_list(I);
to_list(I) when is_binary(I) ->
    erlang:binary_to_list(I);
to_list(I) ->
    I.


%%%===================================================================
%%% Internal functions
%%%===================================================================
