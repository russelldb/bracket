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
%-export([to_json/1, from_json/1]).

-include("bracket.hrl").
-compile(export_all).
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
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
encode(undefined) ->
    "";
encode(T) when is_record(T, rider) ->
    {struct, [{rider, {struct, [{name, T#rider.name}, {seed, T#rider.seed}]}}]};
encode(T) when is_record(T, result) ->
    Winner = encode(T#result.winner),
    {struct, [{result, {struct, [{winner, Winner}, {time1, T#result.time1}, {time2, T#result.time2}]}}]};
encode(T) when is_record(T, match) ->
    {struct, [{match, {struct, [{number, T#match.number}, {rider1, encode(T#match.rider1)}, {rider2, encode(T#match.rider2)}, {result, encode(T#match.result)}]}}]};
encode({round, N, {matches, M}}) ->
    {struct, [{round, {struct, [{number, N}, {matches, encode(M, [])}]}}]}.


encode([], Acc) ->
    {array, lists:reverse(Acc)};
encode([H|T], Acc) ->
    encode(T, [encode(H)|Acc]).

