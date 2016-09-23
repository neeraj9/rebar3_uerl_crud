%%% @doc Defines the behaviour for POST callback modules,
%%% to be used with the `post_handler' pattern.
%%% The behaviour is letting the caller specify how to handle
%%% common functions for POST operations.
%%%
%%% Specify the `-behaviour(post_callback).' module attribute
%%% and the rest can then be implemented.
-module(post_callback).
-ignore_xref([behaviour_info/1]).
-type state() :: term().

-callback init() -> state().
-callback terminate(state()) -> term().
