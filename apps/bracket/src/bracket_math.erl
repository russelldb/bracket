%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% the maths bit
%%% @end
%%% Created :  8 Dec 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(bracket_math).

%% API
%%-export([rounds/1, byes/1, tournament/1, is_pow2/1]).
-compile(export_all).

-include("bracket.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc How many rounds will there be for the given number of competitors
%% @spec rounds(Competitors) -> Rounds::int()
%% @end
%%--------------------------------------------------------------------
rounds(Competitors) when is_integer(Competitors) ->
    ceiling(log2(Competitors)).

%%--------------------------------------------------------------------
%% @doc How many byes will there be for the given number of competitors
%% @spec byes(Competitors) -> Byes::int()
%% @end
%%--------------------------------------------------------------------
byes(Competitors) when is_integer(Competitors) ->
    ceiling(math:pow(2, rounds(Competitors)) - Competitors).


%%--------------------------------------------------------------------
%% @doc Generate the matches for the first round of a square single elimination tournament
%% @spec matches(N) -> {TopHalf, BottomHalf}
%% @end
%%--------------------------------------------------------------------
matches(L) when length(L) =:= 2 ->
    list_to_tuple(L);
matches(L) when is_list(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    L3 = lists:zip(L1, lists:reverse(L2)),
    matches(L3).


%%%===================================================================
%%% Internal functions
%%%===================================================================
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

log2(N) when is_integer(N) ->
    math:log(N) / math:log(2).

is_pow2(X) ->
    ( (X > 0) and  ((X band (X - 1)) == 0) ).

%%Flattens the rounds of a tournment and adds round and match numbers to the data
flatten([], _, _, Tournament) ->
    lists:reverse(Tournament);
flatten([Round|Rounds], MatchCount, RoundCount, Tournament) ->
    L = flatten_matches(Round),
    {Matches, NewMatchCount} = lists:mapfoldl(fun mapfoldlfun/2,
			    MatchCount, L),
    flatten(Rounds, NewMatchCount, RoundCount+1, [{round, RoundCount, {matches, Matches}}|Tournament]).

%%% Matches are deeply nested tuples, this un-nests them
flatten_matches(#rider{}=R) ->
    [{winner, R}];
flatten_matches({#rider{}=R1, #rider{}=R2}) ->
    [{match, R1, R2}];
flatten_matches({T1, T2}) ->
    THM = flatten_matches(T1),
    BHM = flatten_matches(T2),
    lists:flatten([THM] ++ [BHM]).

%%% Used to map/fold the match into a match with a number
mapfoldlfun({match, R1, R2}, MatchCount) ->
    {MatchNumber, NextMatchNumber} = match_number_and_next(MatchCount, R1, R2),
    Match = #match{number=MatchNumber, rider1=R1, rider2=R2, result=generate_result(R1, R2)},
    {Match, NextMatchNumber};
mapfoldlfun(X, MatchCount) ->
    {X, MatchCount}.

match_number_and_next(MatchCount, #rider{name=bye}, _R2) ->
    {0, MatchCount};
match_number_and_next(MatchCount, _R1, #rider{name=bye}) ->
    {0, MatchCount};
match_number_and_next(MatchCount, _, _) ->
    {MatchCount, MatchCount+1}.

%%% Generates a result for the two riders (if one is a bye the other is the winner)
generate_result(#rider{name=bye}, Rider) ->
    #result{winner=Rider};
generate_result(Rider, #rider{name=bye}) ->
    #result{winner=Rider};
generate_result(_Rider1, _Rider2) ->
    #result{}.

