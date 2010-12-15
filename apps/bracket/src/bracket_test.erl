%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% @end
%%% Created :  15 Dec 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(bracket_test).

%% API
-export([generate/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc generates some dummy riders, to cut down on my typing when testing
%% Generates as many riders as specified. Seeds will be allocated in order.
%% @spec generate(N::integer()) -> Riders::list().
%% @end
%%--------------------------------------------------------------------
generate(N) when is_integer(N) ->
    generate(N, 0, []).
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
generate(N, N, Acc) ->
    lists:reverse(Acc);
generate(N, Seed, Acc) ->
    NewSeed = Seed + 1,
    RiderName = "Rider " ++ integer_to_list(NewSeed),
    generate(N, NewSeed, [{rider, {seed, NewSeed}, {name, RiderName}}|Acc]).
