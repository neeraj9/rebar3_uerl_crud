-module(model_{{name}}).
-include_lib("proper/include/proper.hrl").
-export([command/1, initial_state/0, next_state/3,
         precondition/2, postcondition/3]).

-record(state, {db=[]}).
-define(PATH, "/{{name}}/").
-define(HOST, "http://localhost:8001" ++ ?PATH).

%% Copy paste any type spec from the model here!
valid_data() -> #{}.
invalid_data() -> [].

command(State) ->
    oneof(
      [{call, crudcli, read_unknown, [?HOST, new_id(State)]},
       {call, crudcli, create_valid, [?HOST, new_id(State), valid_data()]},
       {call, crudcli, create_invalid, [?HOST, new_id(State), invalid_data()]},
       {call, crudcli, update_unknown, [?HOST, new_id(State), valid_data()]},
       {call, crudcli, delete_unknown, [?HOST, new_id(State)]}]
      ++
      case State#state.db of
          [_|_] ->
              [{call, crudcli, read_exists, [?HOST, existing_id(State)]},
               {call, crudcli, recreate_valid, [?HOST, existing_id(State), valid_data()]},
               {call, crudcli, recreate_invalid, [?HOST, existing_id(State), invalid_data()]},
               {call, crudcli, update_valid, [?HOST, existing_id(State), valid_data()]},
               {call, crudcli, update_invalid, [?HOST, existing_id(State), invalid_data()]},
               {call, crudcli, delete_exists, [?HOST, existing_id(State)]},
               %% putting it lower here since it's a bit of a corner case and
               %% do not need this one to be too frequent
               {call, crudcli, create_valid, [?HOST, "", valid_data()]}];
          [] ->
              []
      end
    ).

%% Initial model value at system start. Should be deterministic.
initial_state() ->
    #state{}.

%% Picks whether a command should be valid under the current state.
precondition(#state{db=List}, {call, _, Fun, [_, Id |_]}) ->
    RequireExistingFuns = [read_exists, recreate_valid, recreate_invalid,
                           update_valid, update_invalid, delete_exists],
    RequireExisting = lists:member(Fun, RequireExistingFuns),
    ExistingId = lists:member(Id, [K || {K,_} <- List]),

    (RequireExisting andalso ExistingId)
    orelse
    (not RequireExisting andalso not ExistingId).

%% Given the state `State' *prior* to the call `{call, Mod, Fun, Args}',
%% determine whether the result `Res' (coming from the actual system)
%% makes sense.
postcondition(#state{db=List}, {call, _, read_exists, [_, Id]}, {Code, _, Body}) ->
    Code =:= 200 andalso {Id, Body} =:= lists:keyfind(Id, 1, List);
postcondition(#state{}, {call, _, read_unknown, _}, {Code, _, Body}) ->
    Code =:= 404 andalso Body =:= undefined;
postcondition(#state{}, {call, _, create_valid, [_, "", _]}, {Code, Headers, Body}) ->
    %% submitting no Id means we create one and show it in the 'location'
    Loc = proplists:get_value("location", Headers),
    (is_list(Loc) andalso lists:prefix(?PATH, Loc))
    andalso
    Code =:= 201 andalso Body =:= undefined;
postcondition(#state{}, {call, _, create_valid, _}, {Code, _, Body}) ->
    Code =:= 204 andalso Body =:= undefined;
postcondition(#state{}, {call, _, recreate_valid, _}, {Code, _, Body}) ->
    Code =:= 204 andalso Body =:= undefined;
postcondition(#state{}, {call, _, create_invalid, _}, {Code, _, Body}) ->
    Code =:= 400 andalso Body =:= undefined;
postcondition(#state{}, {call, _, recreate_invalid, _}, {Code, _, Body}) ->
    Code =:= 400 andalso Body =:= undefined;
postcondition(#state{}, {call, _, update_unknown, _}, {Code, _, Body}) ->
    Code =:= 400 andalso Body =:= undefined;
postcondition(#state{}, {call, _, update_valid, _}, {Code, _, Body}) ->
    Code =:= 204 andalso Body =:= undefined;
postcondition(#state{}, {call, _, update_invalid, _}, {Code, _, Body}) ->
    Code =:= 400 andalso Body =:= undefined;
postcondition(#state{}, {call, _, delete_exists, _}, {Code, _, Body}) ->
    Code =:= 204 andalso Body =:= undefined;
postcondition(#state{}, {call, _, delete_unknown, _}, {Code, _, Body}) ->
    Code =:= 404 andalso Body =:= undefined.

%% Assuming the postcondition for a call was true, update the model
%% accordingly for the test to proceed.
next_state(State=#state{}, _Res, {call, _Mod, create_valid, [_, "", _]}) ->
    %% must extract the id from the location header, which requires evaluation
    %% and non-determinism.
    %% Because of this we can't properly evaluate that result. We rely on the
    %% fact that the ID will hopefully not clash with our generated ones
    %% and ignore it for subsequent runs.
    State;
next_state(State=#state{db=List}, _Res, {call, _Mod, create_valid, [_, Id, Val]}) ->
    State#state{db=lists:keystore(Id, 1, List, {Id,Val})};
next_state(State=#state{db=List}, _Res, {call, _Mod, recreate_valid, [_, Id, Val]}) ->
    State#state{db=lists:keystore(Id, 1, List, {Id,Val})};
next_state(State=#state{db=List}, _Res, {call, _Mod, update_valid, [_, Id, Val]}) ->
    State#state{db=lists:keystore(Id, 1, List, {Id,Val})};
next_state(State=#state{db=List}, _Res, {call, _Mod, delete_exists, [_, Id]}) ->
    State#state{db=lists:keydelete(Id, 1, List)};
next_state(State, _Res, {call, _Mod, _Fun, _Args}) ->
    NewState = State,
    NewState.

%% Generators
new_id(#state{db=List}) ->
    ?SUCHTHAT(Id,
              oneof([url_fragment(), known_keys()]),
              not lists:member(Id, List)).

existing_id(#state{db=List}) ->
    ?LET({K,_}, oneof(List), K).

url_fragment() ->
    non_empty(list(union(
        lists:seq($a,$z) ++ lists:seq($A,$Z) ++ lists:seq($0,$9) ++
        "._~!$&'()*+,;=:@-" ++ ["%1f"]
    ))).

known_keys() ->
    oneof([atom_to_list(?MODULE)++"_key_"++integer_to_list(N)
           || N <- lists:seq(1,50)]).
