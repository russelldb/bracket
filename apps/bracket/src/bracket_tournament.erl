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
-export([tournament/1, add_byes/1, seed/1]).

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
    bracket_math:rounds(Matches, 1, []).

seed(Riders) ->
    seed(Riders, 0, []).

seed([], _, Seeded) ->
    lists:reverse(Seeded);
seed([{rider, {seed, 0}, Name}|Riders], CurrentSeed, Seeded) ->
    Seed = CurrentSeed + 1,
    seed(Riders, Seed, [{rider, {seed, Seed}, Name}|Seeded]);
seed([{rider, {seed, N}, _}=Rider|Riders], _CurrentSeed, Seeded) ->
    seed(Riders, N, [Rider|Seeded]).



add_byes(Riders) ->
    Len = length(Riders),
    case bracket_math:is_pow2(Len) of
	true ->
	    Riders;
	false  ->
	    Riders ++ lists:duplicate(bracket_math:byes(Len), {rider, {seed, 0}, {name, bye}})
    end.
