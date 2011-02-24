%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% turn an internal tournament representation into JSON
%%% @end
%%% Created : 20 Dec 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(bracket_json).

%% API
-export([to_json/1, from_json/1]).

-include("bracket.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc to_json turns a Tournament() into a JSON string
%% @spec to_json(Tournament::tournament()) -> Json:json().
%% @end
%%--------------------------------------------------------------------
to_json(T) ->
    mochijson:encode(encode(T, [])).

from_json(T) ->
    decode(mochijson:decode(T)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
encode([]) ->
    "";
encode(undefined) ->
    "";
encode(T) when is_record(T, rider) ->
    {struct, [{rider, {struct, [{name, T#rider.name}, {seed, T#rider.seed}, {gender, T#rider.gender}]}}]};
encode(T) when is_record(T, result) ->
    Winner = encode(T#result.winner),
    {struct, [{result, {struct, [{winner, Winner}, {time1, T#result.time1}, {time2, T#result.time2}]}}]};
encode(T) when is_record(T, match) ->
    {struct, [{match, {struct, [{number, T#match.number}, {rider1, encode(T#match.rider1)}, {rider2, encode(T#match.rider2)}, {result, encode(T#match.result)}]}}]};
encode({round, N, {matches, M}}) ->
    {struct, [{round, {struct, [{number, N}, {matches, encode(M, [])}]}}]};
encode({champion, Rider}) ->
    {struct, [{champion, {struct, [{rider, encode(Rider)}]}}]}.


encode([], Acc) ->
    {array, lists:reverse(Acc)};
encode([H|T], Acc) ->
    encode(T, [encode(H)|Acc]).

decode({array, A}) ->
    decode(A, []);
decode([{"round", Round}]) ->
   decode_round(Round);
decode([{"match", Match}]) ->
    decode_match(Match);
decode([{"champion", {struct, [{"rider", Champ}]}}]) ->
    {champion, decode_rider(Champ)}.

decode([], Acc) ->
    lists:reverse(Acc);
decode([{struct, Obj}|T], Acc) ->
    decode(T, [decode(Obj)|Acc]).
    


decode_round({struct, Round}) ->
    {round, proplists:get_value("number", Round), {matches, decode(proplists:get_value("matches", Round))}}.

decode_match({struct, Match}) ->
    #match{number=proplists:get_value("number", Match), rider1=decode_rider(proplists:get_value("rider1", Match)), rider2=decode_rider(proplists:get_value("rider2", Match)),
	   result=decode_result(proplists:get_value("result", Match))}.
	      
decode_rider([]) ->
    [];
decode_rider({struct, [{"rider", {struct, Rider}}]}) ->
    #rider{name=proplists:get_value("name", Rider), seed=proplists:get_value("seed", Rider), gender=proplists:get_value("gender", Rider)}.

decode_result({struct, [{"result", {struct, Result}}]}) ->
    #result{winner=decode_rider(proplists:get_value("winner", Result)), time1=proplists:get_value("time1", Result), time2=proplists:get_value("time2", Result)}.
