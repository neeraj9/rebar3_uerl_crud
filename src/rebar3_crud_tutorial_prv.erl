-module(rebar3_crud_tutorial_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, crud).
-define(DEPS, [{default, compile}]).
-define(MANIFEST, ".tutorial.manifest").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {namespace, tutorial},
            {profiles, [test]},
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 tutorial crud"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Erlang tutorial using CRUD templates plugin"},
            {desc, "Erlang tutorial using CRUD templates plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Manifest = restore_manifest(State),
    rebar_utils:update_code(rebar_state:code_paths(State, all_deps), [soft_purge]),
    case run_tutorial(Manifest, State) of
        {ok, NewManifest, NewState} ->
            store_manifest(NewManifest, NewState),
            {ok, NewState};
        Other ->
            Other
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% PRIVATE %%
restore_manifest(State) ->
    case file:consult(?MANIFEST) of
        {ok, [Map]} ->
            Map;
        {error, enoent} ->
            Default = #{step => undefined},
            store_manifest(Default, State),
            Default
    end.

store_manifest(Map, _State) ->
    file:write_file(?MANIFEST, io_lib:format("~p.", [Map])).

run_tutorial(M=#{step := undefined}, State) ->
    %% remove memstore module
    rebar_api:debug("removing backends/memstore.erl", []),
    file:delete("backends/memstore.erl"),
    %% check if current level passes
    rebar_api:debug("checking for fridge model in router:specs()", []),
    Specs = try
                router:specs()
            catch
                Error:Reason ->
                    rebar_api:debug("Specs failed: ~p:~p", [Error, Reason]),
                    []
            end,
    rebar_api:debug("specs: ~p", [Specs]),
    case lists:member(fridge_model, Specs) of
        true ->
            %% Skip to next level
            run_tutorial(M#{step => fridge_handler}, State);
        false ->
            Txt =
            "Hello! This is a tutorial for basic Erlang using CRUD applications. "
            "The first step for you should be to familiarize yourself with basic "
            "Erlang data structures, including lists and recursion. "
            "See http://... for more information.~n"
            "~n"
            "The next step after this will be to add a new handler for the CRUD "
            "resource. Do so by running:~n"
            "~n"
            "\t$ rebar3 new crud-handler fridge~n"
            "~n"
            "You can then add the 'fridge_model' module to 'src/router.erl', "
            "which registers it. Once done, you can then call this command again.",
            rebar_api:info(Txt, []),
            {ok, M, State}
    end;
run_tutorial(M=#{step := fridge_handler}, State) ->
    FileOp = case file:open("backends/memstore.erl", [read]) of
        {error, enoent} ->
            rebar_api:debug("add empty backends/memstore.erl module", []),
            file:write_file("backends/memstore.erl", memstore()),
            added;
        {ok, FD} ->
            rebar_api:debug("memstore.erl already exists", []),
            file:close(FD),
            exists
    end,
    Res = case FileOp of
        added ->
             rebar_api:debug("skipping tests since the file was just added", []),
             false;
        _ ->
            rebar_api:debug("checking proper results for fridge handler.", []),
            run_proper([prop_fridge], State)
    end,
    case Res of
        true ->
            %% Skip to next level
            run_tutorial(M#{step => recipe_handler}, State);
        false ->
            Txt =
            "Good! Now the handler is ready, but sadly has no storage implementation. "
            "I have added an empty file 'backends/memstore.erl' module.~n"
            "~n"
            "Please implement a proper database for a key value store in memory for it. "
            "The database should use a list of tuples, and recursion with pattern "
            "matching to store data. The functions required are:~n"
            "~n"
            "- init(), which returns a fresh, empty database.~n"
            "- create(Key, Val, DB), which returns a new database with the key and the "
            "value stored.~n"
            "- read(Key, DB), which returns either {error, not_found} or {ok, Value}.~n"
            "- update(Key, Value, DB), which modifies an existing entry.~n"
            "- delete(Key, DB), which deletes an existing entry and returns a DB.~n"
            "~n"
            "Compile the code with 'rebar3 compile'. You can run this command again "
            "to know if it worked.",
            rebar_api:info(Txt, []),
            {ok, M, State}
    end;
run_tutorial(M=#{step := recipe_handler}, State) ->
    %% check if current level passes
    rebar_api:debug("checking for recipe model in router:specs()", []),
    Specs = try
                router:specs()
            catch
                Error:Reason ->
                    rebar_api:debug("Specs failed: ~p:~p", [Error, Reason]),
                    []
            end,
    rebar_api:debug("specs: ~p", [Specs]),
    case lists:member(recipe_model, Specs) of
        true ->
            %% Skip to next level
            run_tutorial(M#{step => whatever_handler}, State);
        false ->
            Txt =
            "The next step after this will be to add a new handler for the CRUD "
            "resource and storing recipes, so we do more than just habe a fridge. "
            "Do so by running:~n"
            "~n"
            "\t$ rebar3 new crud-handler recipe~n"
            "~n"
            "You can then add the 'recipe_model' module to 'src/router.erl', "
            "which registers it. Once done, you can then call this command again.",
            rebar_api:info(Txt, []),
            {ok, M, State}
    end;
run_tutorial(M, State) ->
    rebar_api:info("For the next step...", []),
    {ok, M, State}.


memstore() ->
"-module(memstore).
-export([init/0, create/3, read/2, update/3, delete/2]).

init() -> [].

create(K,V,[]) -> [{K,V}].

read(_, []) -> {error, not_found};
read(K, ??) -> {ok, V}.

update(K,V,[]) -> [{K,V}].

delete(_, []) -> [].
".

run_proper(Mods, State) ->
    NewState = rebar_state:set(State, proper_opts, [{module, Mods},{numtests,25}]),
    case rebar3_proper_prv:do(NewState) of
        {ok, _State} -> true;
        _ -> false
    end.
