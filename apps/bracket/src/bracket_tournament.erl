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
-export([tournament/1, update/1]).

-include("bracket.hrl").

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
    rounds(R1, bracket_math:rounds(length(Riders)) + 1).

%%--------------------------------------------------------------------
%% @doc update takes an existing tournament and generates the matches for any results
%% @spec update(Tournament::tournament()) -> Tournament::tournament()
%% @end
%%--------------------------------------------------------------------
update(Tournament) ->
    Rounds = started_rounds(Tournament, []),
    rounds(Rounds, length(Tournament), highest_match(hd(Rounds))).

%%%===================================================================
%%% Internal functions
%%%===================================================================
started_rounds([H|T], Acc) ->
    case started(H) of 
	true ->
	    started_rounds(T, [H|Acc]);
	false ->
	    Acc
    end.

%%% Has the given round been started?
started({round, _, {matches, M}}) ->
    has_winners(M).

%%% does the list of matches have any winners?
has_winners([]) ->
    false;
has_winners([#match{result=#result{winner=[]}}|T]) ->
    has_winners(T);
has_winners([#match{result=#result{winner=#rider{}}}|_]) ->
    true;
has_winners([{champion, _}]) ->
    false.


%%% Really need to randomise the seeding of the unseeded riders...assumes a "sorted" list with lowest seed (1) upto highest (0)
seed(Riders) ->
    seed(Riders, 0, []).

%%% If any riders have a zero (0) seed, give them the next available seed (Zero is unseeded)
seed([], _, Seeded) ->
    lists:reverse(Seeded);
seed([#rider{seed=0}=R|Riders], CurrentSeed, Seeded) ->
    Seed = CurrentSeed + 1,
    seed(Riders, Seed, [R#rider{seed=Seed}|Seeded]);
seed([#rider{seed=N}=Rider|Riders], _CurrentSeed, Seeded) ->
    seed(Riders, N, [Rider|Seeded]).

%%% Add any byes needed to the list of riders. Are added to the end of the list and they are all
%%% Unseeded
add_byes(Riders) ->
    Len = length(Riders),
    case bracket_math:is_pow2(Len) of
	true ->
	    Riders;
	false  ->
	    Riders ++ lists:duplicate(bracket_math:byes(Len), #rider{seed=0, name=bye})
    end.

%%% Set up the match count
rounds([{round, 1, {matches, M}}]=Rounds, RCount) ->
    rounds(Rounds, RCount, highest(M)).

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
next_matches([#match{}=M], MatchCount, Acc) ->
    next_matches([], MatchCount, [{champion, winner(M)}|Acc]);
next_matches([#match{}=Match1, #match{}=Match2|Matches], MatchCount, Acc) ->
    NextMatchNum = MatchCount +1,
    Match = #match{number=NextMatchNum, rider1=winner(Match1), rider2=winner(Match2), result=#result{}},
    next_matches(Matches, NextMatchNum, [Match|Acc]).

%%% Returns the higher of two seeded riders
higher_seed(#rider{seed=A}=R1, #rider{seed=B}) when A =< B ->
    R1;
higher_seed(#rider{seed=A}, #rider{seed=B}=R2) when B =< A ->
    R2.

%%% winner or highest seed
winner(#match{rider1=R1, result=#result{winner=R1}}) ->
    R1;
winner(#match{rider2=R2, result=#result{winner=R2}}) ->
    R2;
winner(#match{number=MatchNumber, rider1=R1, rider2=R2, result=_}) ->
    #rider{seed=RiderSeed} = higher_seed(R1, R2),
    #rider{name="Winner of match " ++ integer_to_list(MatchNumber), seed=RiderSeed}.

%%% Get the highest numbered match in the given round
highest_match({round, _, {matches, M}}) ->
    highest(M).

%%% Get the highest match in the list of matchs in a round. (so we can get match numbers for subsequent rounds)
highest(M) ->
    highest(M, 0).

%%% Helper for highest
highest([], Highest) ->
    Highest;
highest([#match{number=N}|T], CurrentHighest) when N > CurrentHighest ->
    highest(T, N);
highest([#match{number=N}|T], CurrentHighest) when N =< CurrentHighest ->
    highest(T, CurrentHighest).
