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
    to_list/1,
    to_binary/1,
    get_value_from_record_list/4,
    get_value_from_record_list/5
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


to_list(E) when is_integer(E) ->
    erlang:integer_to_list(E);
to_list(E) when is_float(E) ->
    erlang:float_to_list(E);
to_list(E) when is_binary(E) ->
    erlang:binary_to_list(E);
to_list(E) ->
    E.

to_binary(E) when is_binary(E) ->
    E;
to_binary(E) when is_list(E) ->
    erlang:list_to_binary(E);
to_binary(E) when is_integer(E) orelse is_float(E) ->
    erlang:list_to_binary(to_list(E));
to_binary(E) ->
    E.

get_value_from_record_list(Key, KeyPos, ValPos, List) ->
    get_value_from_record_list(Key, KeyPos, ValPos, List, unknown).

get_value_from_record_list(Key, KeyPos, ValPos, [H | _Tail] = List, Default) when is_tuple(H) ->
    case lists:keyfind(Key, KeyPos, List) of
        false ->
            Default;
        Record ->
            erlang:element(ValPos, Record)
    end;
get_value_from_record_list(_Key, _KeyPos, _ValPos, _List, _Default) ->
    {error, badarg}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
