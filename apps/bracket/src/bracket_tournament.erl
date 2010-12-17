%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% Tournament generation
%%% @end
%%% Created :  14 Dec 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------

-module(bracket_tournament).

%% API
-export([tournament/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc tournament generates the matches for the given riders
%% @spec tournament(Riders::riders()) -> Matches:matches()
%% @end
%%--------------------------------------------------------------------
tournament(Riders) when is_list(Riders) ->
    Riders2 = add_byes(Riders),
    Seeded = seed(Riders2),
    Matches = bracket_math:matches(Seeded),
    R1 = bracket_math:flatten([Matches], 1, 1, []),
    rounds(R1, bracket_math:rounds(length(Riders))).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Really need to randomise the seeding of the unseeded riders...
seed(Riders) ->
    seed(Riders, 0, []).

%%% If any riders have a zero (0) seed, give them the next available seed (Zero is unseeded)
seed([], _, Seeded) ->
    lists:reverse(Seeded);
seed([{rider, {seed, 0}, Name}|Riders], CurrentSeed, Seeded) ->
    Seed = CurrentSeed + 1,
    seed(Riders, Seed, [{rider, {seed, Seed}, Name}|Seeded]);
seed([{rider, {seed, N}, _}=Rider|Riders], _CurrentSeed, Seeded) ->
    seed(Riders, N, [Rider|Seeded]).

%%% Add any byes needed to the list of riders. Are added to the end of the list and they are all
%%% Unseeded
add_byes(Riders) ->
    Len = length(Riders),
    case bracket_math:is_pow2(Len) of
	true ->
	    Riders;
	false  ->
	    Riders ++ lists:duplicate(bracket_math:byes(Len), {rider, {seed, 0}, {name, bye}})
    end.

rounds([{round, 1, {matches, M}}]=Rounds, RCount) ->
    rounds(Rounds, RCount, length(M)).

%%% Generates the rest of the rounds for the given list of Rounds
rounds(Rounds, RCount, _) when is_list(Rounds), length(Rounds) =:= RCount ->
    lists:reverse(Rounds);
rounds([Round|_]=Rounds, RCount, MatchCount) ->
    {NextRound, NewMatchCount} = next_round(Round, MatchCount), 
    rounds([NextRound|Rounds], RCount, NewMatchCount).

%%% Takes a Term::round and generates the next round from it
next_round({round, RoundNum, {matches, Matches}}, MatchCount) ->
    NextMatches = next_matches(Matches, MatchCount, []),
   {{round, RoundNum+1, {matches, NextMatches}}, MatchCount + length(NextMatches)}.

%%% Generates the next matches from a list of matches
next_matches([], _, Acc) ->
    lists:reverse(Acc);
next_matches([{match, _, {riders, Rider1, Rider2}}], MatchCount, Acc) ->
    next_matches([], MatchCount, [{winner, higher_seed(Rider1, Rider2)}|Acc]);
next_matches([{match, _, {riders, Rider1, Rider2}}, {match, _, {riders, Rider3, Rider4}}|Matches], MatchCount, Acc) ->
    NextMatchNum = MatchCount +1,
    Match = {match, NextMatchNum, {riders, higher_seed(Rider1, Rider2), higher_seed(Rider3, Rider4)}},
    next_matches(Matches, NextMatchNum, [Match|Acc]).

%%% Returns the higher of two seeded riders
higher_seed({rider, {seed, A}, _}=R1, {rider, {seed, B}, _}) when A =< B ->
    R1;
higher_seed({rider, {seed, A}, _}, {rider, {seed, B}, _}=R2) when B =< A ->
    R2.
