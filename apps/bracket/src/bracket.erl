%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc bracket startup code

-module(bracket).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    bracket_sup:start_link().

%% @spec start() -> ok
%% @doc Start the bracket server.
start() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(bracket).

%% @spec stop() -> ok
%% @doc Stop the bracket server.
stop() ->
    Res = application:stop(bracket),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
