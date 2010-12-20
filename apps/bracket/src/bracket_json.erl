%%%-------------------------------------------------------------------
%%% @author Russell Brown <russell@ossme.net>
%%% @copyright (C) 2010, Russell Brown
%%% @doc
%%% turn an internal tournament representation into JSON
%%% @end
%%% Created : 20 Dec 2010 by Russell Brown <russell@ossme.net>
%%%-------------------------------------------------------------------
-module(bracket_json).

%% API
-export([to_json/1, from_json/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc to_json turns a Tournament() into a JSON string
%% @spec to_json(Tournament::tournament()) -> Json:json().
%% @end
%%--------------------------------------------------------------------
to_json(T) ->
    ok.

from_json(T) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
