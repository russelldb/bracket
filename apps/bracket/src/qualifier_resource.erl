%% @author Russell Brown <russell@ossme.net>
%% @copyright 2010 Russell Brown.
%% @doc qualifier resource, takes name/gender and
%% Adds them to SQLLite
%% Creates a bunch of matches (random(ish) pairings)
%% After a race gets the fastest times for the two riders and displays them 
%% At the end of the qualifier displays the times in order
%% Generates a bracket based on the fastest N times

-module(qualifier_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2, post_is_create/2, process_post/2]).
-export([process_put/2, content_types_accepted/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("bracket.hrl").

init([]) -> 
    Res = sqlite3:open(gfx, [{db, "/Users/russell/Library/Preferences/GoldsprintsFX.150CD2418696DCCD52376108DD815BE3EE017F77.1/Local Store/GFX2_3.DB"}]),
    GFX = case Res of
	      {ok , Pid} -> Pid;
	      {error, {already_started, Pid}} -> Pid;
	      _ -> erlang:error(slqlite_fail)
	  end,
    {ok, [{pid, GFX}]}.

to_json(ReqData, State) ->
    Size = get_qs_value("n", ReqData, 16),
    try list_to_integer(Size) of
	SizeInt ->
	    {bracket_json:to_json(bracket_tournament:tournament(bracket_test:generate(SizeInt))), ReqData, State}
    catch
	error:badarg ->
	    {{halt, 400}, ReqData, State}
    end.

content_types_provided(Req, Context) ->
    {[{"text/html", to_json}], Req, Context}.

allowed_methods(Req, Context) ->
    {['POST'], Req, Context}.

post_is_create(Req, Context) ->
    {false, Req, Context}.

process_post(Req, Context) ->
    Data = wrq:req_body(Req),
    Data1 = mochiweb_util:parse_qs(Data),
    Riders = proplists:get_all_values("rider", Data1),
    Seeds = proplists:get_all_values("seed", Data1),
    Genders = proplists:get_all_values("gender", Data1, "M"),
    Entrants = lists:zipwith3(fun(X, Y, Z) -> #rider{name=X, seed=list_to_integer(Y), gender=gender(Z)} end, Riders, Seeds, Genders),
    SortedEntrants = sort(Entrants),
    write_to_db(Entrants),
    T = bracket_tournament:tournament(SortedEntrants),
    JSON = bracket_json:to_json(T),
    {true, wrq:set_resp_body(JSON, Req), Context}.

gender("M") ->
    "M";
gender("F") ->
    "F";
gender(_) ->
    "M".

content_types_accepted(Req, Context) ->
    {[{"application/x-www-form-urlencoded", process_post}], Req, Context}.

get_qs_value(Key, ReqData, Default) ->
    get_qs_value(wrq:get_qs_value(Key, ReqData), Default).

get_qs_value(undefined, Default) ->
    Default;
get_qs_value(Val, _Default) ->
    Val.

sort(Entrants) ->
    lists:sort(fun sort/2, Entrants).

sort(#rider{seed=_A}, #rider{seed=0}) ->
    true;
sort(#rider{seed=0}, #rider{seed=_B}) ->
    false;
sort(#rider{seed=A}, #rider{seed=B}) ->
    A =< B.

write_to_db([]) ->
    ok;
write_to_db([#rider{name=Name, gender=Gender}|Riders]) ->
    sqlite3:write(gfx, 'Roster', [{name, Name}, {gender, Gender}, {time, "00:00:000"}]),
    write_to_db(Riders).

