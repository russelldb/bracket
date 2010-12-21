%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the bracket application.

-module(bracket_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for bracket.
start(_Type, _StartArgs) ->
    bracket_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for bracket.
stop(_State) ->
    ok.
