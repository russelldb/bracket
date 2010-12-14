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
%% @doc builds a datastructure of all the matches in a tournament.
%% @spec tournament(N::integer) -> [Matches::term()]
%% @end
%%--------------------------------------------------------------------
tournament(N) when is_integer(N) ->
    case is_pow2(N) of
	false ->
	    tournament(N + byes(N));
	true ->
	    Matches = matches(N),
	    rounds(Matches, [])
    end.


rounds({rider, _, _}=R, Acc) ->
    lists:reverse([R|Acc]);
rounds(M, Acc) ->
    rounds(next_round(M), [M|Acc]).


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
    matches(L3);
matches(N) when is_integer(N) ->
    matches(lists:seq(1, N)).

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
