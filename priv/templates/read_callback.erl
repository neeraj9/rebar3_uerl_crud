%%% @doc Defines the behaviour for READ callback modules,
%%% to be used with the `read_handler' pattern.
%%% The behaviour is letting the caller specify how to handle
%%% common functions for CRUD operations.
%%%
%%% Specify the `-behaviour(read_callback).' module attribute
%%% and the rest can then be implemented.
-module(read_callback).
-ignore_xref([behaviour_info/1]).
-type state() :: term().

-callback init() -> state().
-callback terminate(state()) -> term().
