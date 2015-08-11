%%% @author Roman Dayneko <roman.dayneko@gmail.com>
%%% @copyright (C) 2015, Roman Dayneko
%%% @doc
%%%
%%% @end
%%% Created : 13 May 2015 by Roman Dayneko <roman.dayneko@gmail.com>
%%%
%%% thanks this article https://arifishaq.wordpress.com/2014/11/25/starting-to-play-with-xmerl/
%%%

-module(eveapi_parser).

-include_lib("xmerl/include/xmerl.hrl").
-include("include/log.hrl").

%% API
-export([encode/1, encode/2, convert_value/1]).

-define(TO_INT_KEYS,
        #{keys =>
              [
               flag,
               itemID,
               quantity,
               singleton,
               typeID,
               locationID,
               code,
               certificateID,
               allianceID,
               augmentatorValue,
               charisma,
               intelligence,
               memory,
               perception,
               willpower,
               characterID,
               cloneSkillPoints,
               corporationID,
               roleID,
               titleID,
               level,
               skillpoints,
               typeID,
               unpublished,
               factionID,
               cloneTypeID,
               jumpCloneID,
               freeRespecs,
               freeSkillPoints,
               homeStationID,
               published,
               rawQuantity,
               ancestryID,
               bloodLineID
              ],
          convert_fun => fun
                             (E) when is_list(E) ->
                                 erlang:list_to_integer(E);
                             (E) -> E
                         end
         }
       ).
-define(TO_BIN_KEYS,
        #{keys =>
              [
               text,
               'DoB',
               allianceName,
               ancestry,
               augmentatorName,
               bloodLine,
               cloneName,
               corporationName,
               roleName,
               titleName,
               gender,
               name,
               race,
               currentTime,
               cachedUntil,
               factionName,
               typeName,
               jumpActivation,
               jumpFatigue,
               jumpLastUpdate,
               lastRespecDate,
               lastTimedRespec,
               remoteStationDate,
               cloneJumpDate
              ],
          convert_fun => fun
                             (E) when is_list(E) ->
                                 erlang:list_to_binary(E);
                             (E) -> E
                         end
         }
       ).
-define(TO_FLOAT_KEYS,
        #{keys =>
              [
               balance
              ],
          convert_fun => fun
                             (E) when is_list(E) ->
                                 erlang:list_to_float(E) ;
                             (E) -> E
                         end
         }
       ).

-define(CONVERT, [?TO_INT_KEYS, ?TO_BIN_KEYS, ?TO_FLOAT_KEYS]).
-define(EMPTY_FUN, fun(E) -> E end).
-define(FALLBACK_CONVERT_FUN, fun(E) -> erlang:list_to_binary(E) end).

%%%===================================================================
%%% API
%%%===================================================================

encode(B) when is_binary(B) ->
    encode(map, erlang:binary_to_list(B));
encode(L) when is_list(L) ->
    encode(map, L).

encode(Type, B) when is_binary(B) ->
    encode(Type, erlang:binary_to_list(B));
encode(plist, []) ->
    {ok, []};
encode(plist, L) when is_list(L) ->
    {Result, _} =
        xmerl_scan:string(L,
                          [
                           {hook_fun, fun element_hook/2},
                           {acc_fun, fun acc_hook/3},
                           {space, normalize},
                           {encoding, "utf-8"}
                          ]
                         ),
    {ok, Result};
encode(map, L) when is_list(L) ->
    case encode(plist, L) of
        {ok, Result} ->
            {ok, plist_to_map([Result])};
        Error  ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

element_hook(#xmlElement{name = error, attributes = [#xmlAttribute{name = code, value = Code}], content = [Text]}, GlobalState) ->
    {{error, [{code, convert_value({code, Code})}, {text, convert_value({text, Text})}]}, GlobalState};
element_hook(#xmlElement{name = eveapi, content = Content, attributes = Attributes}, GlobalState) ->
    ApiVersion = erlang:list_to_integer(helper:get_value_from_record_list(version, #xmlAttribute.name, #xmlAttribute.value, Attributes, "0")),
    {{eveapi, [{version, ApiVersion} | Content]}, GlobalState};
element_hook(#xmlElement{name = key, content = Content, attributes = Attributes}, GlobalState) ->
    APIKeyType = helper:to_binary(helper:get_value_from_record_list(type, #xmlAttribute.name, #xmlAttribute.value, Attributes)),
    APIKeyExpirationDate = helper:to_binary(helper:get_value_from_record_list(expires, #xmlAttribute.name, #xmlAttribute.value, Attributes)),
    {{apikey, [{type, APIKeyType}, {expires, APIKeyExpirationDate} | Content]}, GlobalState};
element_hook(#xmlElement{name = rowset, content = Content, attributes = Attributes}, GlobalState) ->
    RowsetName = erlang:list_to_atom(helper:get_value_from_record_list(name, #xmlAttribute.name, #xmlAttribute.value, Attributes, "unknown")),
    {{RowsetName, [Content]}, GlobalState};
element_hook(#xmlElement{name = row, content = Content, attributes = Attributes}, GlobalState) ->
    AttributesPList = [{Name, convert_value({Name, Value})} || #xmlAttribute{name = Name, value = Value} <- Attributes],
    Result =
        case Content of
            [] ->
                AttributesPList;
            [Content1] ->
                [Content1 | AttributesPList]
        end,
    {Result, GlobalState};
element_hook(#xmlElement{name = Name, content = Content, attributes = []}, GlobalState) ->
    {{Name, Content}, GlobalState};
element_hook(#xmlText{value = Value}, GlobalState) ->
    {Value, GlobalState};
element_hook(Entity, GlobaState) ->
    ?DBG("doesn't match ~p", [Entity]),
    {Entity, GlobaState}.

acc_hook(" ", Acc, GlobalState) ->
    {Acc, GlobalState};
acc_hook({certificates, Certificates}, Acc, GlobalState) ->
    {[{certificates, [convert_value({certificateID, Id}) || [{'certificateID', Id}] <- Certificates]} | Acc], GlobalState};
acc_hook({Key, [{_, _} | _Tail] = Value}, Acc, GlobalState) when is_list(Value) ->
    {[{Key, [{Key1, convert_value({Key1, Value1})} || {Key1, Value1} <- Value]} | Acc], GlobalState};
acc_hook({Key, [Value]}, Acc, GlobalState) when is_list(Value) ->
    {[{Key, convert_value({Key, Value})} | Acc], GlobalState};
acc_hook({Key, _Value} = Element, Acc, GlobalState) ->
    {[{Key, convert_value(Element)} | Acc], GlobalState};
acc_hook(Entity, Acc, GlobalState) ->
    {[Entity | Acc], GlobalState}.

plist_to_map([]) ->
    [];
plist_to_map([H | _] = ListOfLists) when is_list(H) ->
    [plist_to_map(List) || List <- ListOfLists];
plist_to_map([{_, _} | _] = PList) ->
    maps:from_list([{K, plist_to_map(V)} || {K, V} <- PList]);
%%  #{Key => plist_to_map(Value) || Key := Value <- M};
%%  bugged maps: syntax error before: '||'
plist_to_map(Element) ->
    Element.

convert_value({Key, Value}) when is_list(Value) ->
    ConvertFun = find_convert_fun(Key, ?CONVERT),
    try ConvertFun(Value) of
        ConvertedValue ->
            ConvertedValue
    catch
        _:_ -> ?FALLBACK_CONVERT_FUN(Value)
    end;
convert_value({_Key, Value}) ->
    Value.

find_convert_fun(Key, [#{keys := Keys, convert_fun := ConvertFun} | Tail]) ->
    case lists:member(Key, Keys) of
        true ->
            ConvertFun;
        _ ->
            find_convert_fun(Key, Tail)
    end;
find_convert_fun(_, []) ->
    ?EMPTY_FUN.
