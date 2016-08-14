%%% @doc Defines the behaviour for CRUD callback modules,
%%% to be used with the `crud_handler' pattern.
%%% The behaviour is letting the caller specify how to handle
%%% common functions for CRUD operations.
%%%
%%% Specify the `-behaviour(crud_callback).' module attribute
%%% and the rest can then be implemented.
-module(crud_callback).
-ignore_xref([behaviour_info/1]).
-type state() :: term().
-type id() :: binary().
-export_type([id/0]).

-callback init() -> state().
-callback terminate(state()) -> term().
-callback validate(term(), state()) -> {boolean(), state()}.
-callback create(id() | undefined, term(), state()) ->
            {false | true | {true, id()}, state()}.
-callback read(id(), state()) -> {{ok, term()} | {error, not_found}, state()}.
-callback update(id(), term(), state()) -> {boolean(), state()}.
-callback delete(id(), state()) -> {boolean(), state()}.
