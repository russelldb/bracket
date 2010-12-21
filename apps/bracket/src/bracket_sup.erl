%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Supervisor for the bracket application.

-module(bracket_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
     %% Read config...
    {ok, BindAddress} = application:get_env(bracket, bind_address),
    {ok, Port} = application:get_env(bracket, port),
    {ok, Name} = application:get_env(bracket, server_name),
    {ok, DispatchFile} = application:get_env(bracket, dispatch),
    {ok, LogDir} = application:get_env(bracket, log_dir),

    {ok, Dispatch} = file:consult(DispatchFile),

    WebConfig = [
                 {ip, BindAddress},
                 {port, Port},
                 {log_dir, LogDir},
                 {dispatch, Dispatch},
		 {name, Name}],

    Web = {webmachine_mochiweb,
           {webmachine_mochiweb, start, [WebConfig]},
           permanent, 5000, worker, dynamic},

  
    Processes = [Web],
    {ok, { {one_for_one, 10, 10}, Processes} }.
