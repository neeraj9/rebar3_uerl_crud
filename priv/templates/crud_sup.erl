%%% @doc
%%% Supervisor for the CRUD app. By default, will boot
%%% the supervision structure required for more persistent
%%% state of the application.
%%% @end
-module({{name}}_sup).

-behaviour(supervisor).

-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%
%%% API FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%
-spec start_link([module()]) -> {ok, pid()} | ignore | {error, term()}.
start_link(Models) ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, Models).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% SUPERVISOR CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(Specs) ->
    {ok, { {one_for_one, 1000, 3600},
          [{Name,
            {memstore_proc, start_link, [Name]},
            permanent, 5000, worker, [memstore_proc]}
           || Name <- Specs]}}.

