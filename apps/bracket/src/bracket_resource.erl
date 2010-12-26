%% @author Russell Brown <russell@ossme.net>
%% @copyright 2010 Russell Brown.
%% @doc basic bracket_resource.

-module(bracket_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

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
    {['GET'], Req, Context}.

get_qs_value(Key, ReqData, Default) ->
    get_qs_value(wrq:get_qs_value(Key, ReqData), Default).

get_qs_value(undefined, Default) ->
    Default;
get_qs_value(Val, _Default) ->
    Val.
    
