%% @author Russell Brown <russell@ossme.net>
%% @copyright 2010 Russell Brown.
%% @doc basic bracket_resource.

-module(bracket_resource).
-export([init/1, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, undefined}.

to_json(ReqData, State) ->
    {bracket_json:to_json(bracket_tournament:tournament(bracket_test:generate(32))), ReqData, State}.

content_types_provided(Req, Context) ->
    {[{"application/json", to_json}], Req, Context}.

allowed_methods(Req, Context) ->
    {['GET'], Req, Context}.
