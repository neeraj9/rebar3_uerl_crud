-module(prop_{{name}}).
-include_lib("proper/include/proper.hrl").
-define(MODEL, model_{{name}}).

prop_test() ->
    _ = start_apps(),
    ?FORALL(Cmds, commands(?MODEL),
            begin
                {History, State, Result} = run_commands(?MODEL, Cmds),
                flush_db(),
                ?WHENFAIL(io:format("History: ~p\nState: ~p\nResult: ~p\n",
                                    [History,State,Result]),
                          aggregate(command_names(Cmds), Result =:= ok))
            end).

%seq_len(Cmds) ->
%    [length(Cmds)].

%seq_range(Cmds) ->
%    N = length(Cmds),
%    Tens = trunc(N / 10)*10,
%    [{Tens,Tens+9}].

%unique_cmds(Cmds) ->
%    [length(lists:usort( [Call || {set, _, {call, _, Call, _}} <- Cmds]))].


flush_db() ->
    memstore_proc:reset({{name}}_model).

start_apps() ->
    application:set_env(lager, log_root, "_build/test/log/", [{persistent, true}]),
    {ok, Apps1} = application:ensure_all_started({{appname}}),
    {ok, Apps2} = application:ensure_all_started(inets),
    Apps1 ++ Apps2.
