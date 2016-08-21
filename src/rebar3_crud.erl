-module(rebar3_crud).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_crud_prv:init(State),
    {ok, State2} = rebar3_crud_tutorial_prv:init(State1),
    {ok, State2}.
