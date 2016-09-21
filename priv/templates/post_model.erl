-module({{name}}_model).
-behaviour(trails_handler).
-behaviour(post_callback).
-export([trails/0]). % define the API for docs
-export([init/0, terminate/1
         ]).
-export_type([{{name}}/0]).


%% example:
%% #{name := binary(),              % must be present
%%   age  => non_negative_integer() % optional
%%  }.
-type {{name}}() :: #{}.
-type state() :: nostate.

%%%%%%%%%%%%%%%%%%%%%%%
%%% API DEFINITIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
%% See: http://swagger.io/specification/
trails() ->
    Path = "/{{name}}",
    Handler = {{name}}_handler,
    HandlerState = [?MODULE],
    Meta = Handler:paths("{{name}}"),
    [trails:trail(Path, Handler, HandlerState, Meta)].


%%%%%%%%%%%%%%%%%%%%%%
%%% CRUD callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%

%% @doc Initialize the state that the handler will carry for
%% a specific request throughout its progression. The state
%% is then passed on to each subsequent call to this model.
-spec init() -> state().
init() ->
    nostate.

%% @doc At the end of a request, the state is passed back in
%% to allow for clean up.
-spec terminate(state()) -> term().
terminate(_State) ->
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%
