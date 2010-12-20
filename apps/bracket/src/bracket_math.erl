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


%%--------------------------------------------------------------------
%% @doc All the rounds following the starting matches
%% @spec rounds(1stRound::term()) -> {Rounds::term()}
%% @end
%%--------------------------------------------------------------------
rounds({rider, _, _}=R, Acc) ->
    lists:reverse([R|Acc]);
rounds(M, Acc) ->
    rounds(next_round(M), [M|Acc]).

%%--------------------------------------------------------------------
%% @doc next_round the matches for the all the following round of  param Round
%% @spec next_round(Round::term()) -> {Rounds::term()}
%% @end
%%--------------------------------------------------------------------
next_round({{rider, {seed, A}, _}=R1, {rider, {seed, B}, _}=R2}) ->
    case smaller({A, B}) of
	A ->
	    R1;
	B ->
	    R2
    end;
next_round({T, B}) ->
    {next_round(T), next_round(B)}.

smaller({TH, BH}) when is_integer(TH), is_integer(BH), TH =< BH ->
    TH;
smaller({TH, BH}) when is_integer(TH), is_integer(BH), BH < TH ->
    BH.

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

%%% Take the tournament and make it flat
flatten_tournament(Tournament) ->
    flatten(Tournament, 1, 1, []).

%%Flattens the rounds of a tournment and adds round and match numbers to the data
flatten([], _, _, Tournament) ->
    lists:reverse(Tournament);
flatten([Round|Rounds], MatchCount, RoundCount, Tournament) ->
    L = flatten_matches(Round),
    {Matches, NewMatchCount} = lists:mapfoldl(fun mapfoldlfun/2,
			    MatchCount, L),
    flatten(Rounds, NewMatchCount, RoundCount+1, [{round, RoundCount, {matches, Matches}}|Tournament]).

%%% Matches are deeply nested tuples, this un-nests them
flatten_matches({rider, _, _}=R) ->
    [{winner, R}];
flatten_matches({{rider, _, _}=R1, {rider, _, _}=R2}) ->
    [{match, R1, R2}];
flatten_matches({T1, T2}) ->
    THM = flatten_matches(T1),
    BHM = flatten_matches(T2),
    lists:flatten([THM] ++ [BHM]).

%%% Used to map/fold the match into a match with a number
mapfoldlfun({match, R1, R2}, MatchCount) ->
    {{match, MatchCount, {riders, R1, R2}}, MatchCount+1};%%% add winner tuple, and add a winner if there is a bye rider
mapfoldlfun(X, MatchCount) ->
    {X, MatchCount}.

