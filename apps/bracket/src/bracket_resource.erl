%% @author Russell Brown <russell@ossme.net>
%% @copyright 2010 Russell Brown.
%% @doc basic bracket_resource.

-module(bracket_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2, post_is_create/2, process_post/2]).
-export([process_put/2, content_types_accepted/2]).

-include_lib("webmachine/include/webmachine.hrl").

-include("bracket.hrl").

init([]) -> {ok, undefined}.

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
    {[{"application/json", to_json}], Req, Context}.

allowed_methods(Req, Context) ->
    {['GET','POST', 'PUT'], Req, Context}.

post_is_create(Req, Context) ->
    {false, Req, Context}.

process_post(Req, Context) ->
    Data = wrq:req_body(Req),
    Data1 = mochiweb_util:parse_qs(Data),
    Riders = proplists:get_all_values("rider", Data1),
    Seeds = proplists:get_all_values("seed", Data1),
    Entrants = lists:zipwith(fun(X, Y) -> #rider{name=X, seed=list_to_integer(Y)} end, Riders, Seeds),
    SortedEntrants = sort(Entrants),
    T = bracket_tournament:tournament(SortedEntrants),
    JSON = bracket_json:to_json(T),
    {true, wrq:set_resp_body(JSON, Req), Context}.

process_put(Req, Context) ->
    Data = wrq:req_body(Req),
    T = bracket_json:from_json(Data),
    error_logger:info_msg("T = ~p~n", [T]),
    T2 = bracket_tournament:update(T),
    error_logger:info_msg("Updated = ~p~n", [T2]),
    JSON = bracket_json:to_json(T2),
    {true, wrq:set_resp_body(JSON, Req), Context}.

content_types_accepted(Req, Context) ->
  {[{"application/x-www-form-urlencoded", process_post}, {"application/json", process_put}], Req, Context}.

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
		       
