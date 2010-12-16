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
    tourny(Seeded).

%%%===================================================================
%%% Internal functions
%%%===================================================================
tourny(Riders) ->
    Matches = bracket_math:matches(Riders),
    Tourny = bracket_math:rounds(Matches, []),
    bracket_math:flatten_tournament(Tourny).

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

%%% Takes a Term::round and generates the next round from it
next_round(Round) ->
    next_matches(Round, []).



    
