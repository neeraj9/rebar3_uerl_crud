%%% @doc a generic CRUD handler based on cowboy's rest framework.
-module({{name}}_handler).

%% Docs callbacks
-export([paths/1]).
%% cowboy_rest callbacks
-export([init/3, rest_init/2, rest_terminate/2, service_available/2,
         allowed_methods/2, resource_exists/2, is_authorized/2,
         content_types_provided/2, to_json/2, delete_resource/2,
         content_types_accepted/2, from_json/2, is_conflict/2]).
%% Don't warn on known optional callbacks
-ignore_xref([paths/1, init/3, rest_init/2, rest_terminate/2,
              service_available/2, allowed_methods/2, resource_exists/2,
              is_authorized/2, content_types_provided/2, to_json/2,
              delete_resource/2, content_types_accepted/2, from_json/2,
              is_conflict/2]).

-record(state, {model :: module(),
                start = erlang:monotonic_time(milli_seconds),
                model_state :: term(),
                resource_id :: binary() | undefined,
                resource :: map() | undefined}).

-define(MIMETYPE, {<<"application">>, <<"json">>, '*'}).
-define(MIMETYPE_DOC, "application/json").

paths(Item) when is_binary(Item) ->
    paths(unicode:characters_to_list(Item));
paths(Item) when is_atom(Item) ->
    paths(atom_to_list(Item));
paths(Item) when is_list(Item) ->
    #{post => #{
        tags => [Item],
        summary => "Creates an object of type "++Item++".",
        description => "Creates an object of type "++Item++".",
        consumes => [?MIMETYPE_DOC],
        produces => [?MIMETYPE_DOC],
        responses => #{
            <<"201">> => #{description => "The object was created without "
                                          "providing an id in the path."},
            <<"204">> => #{description => "The object was created at the "
                                          "specified path."},
            <<"400">> => #{description => "The object is invalid."}
        }
      },
      get => #{
        tags => [Item],
        summary => "Reads an object of type "++Item++".",
        description => "Reads an object of type "++Item++".",
        consumes => [?MIMETYPE_DOC],
        produces => [?MIMETYPE_DOC],
        responses => #{
            <<"200">> => #{description => "The object was found and is included in "
                                          "the content body."},
            <<"404">> => #{description => "The object is not found."}
        }
      },
      put => #{
        tags => [Item],
        summary => "Updates an existing object of type "++Item++".",
        description => "Updates an existing object of type "++Item++".",
        consumes => [?MIMETYPE_DOC],
        produces => [?MIMETYPE_DOC],
        responses => #{
            <<"204">> => #{description => "The object was modified successfully."},
            <<"400">> => #{description => "The object is invalid, or the update "
                                          "operation not legal."}
        }
      },
      delete => #{
        tags => [Item],
        summary => "Deletes an existing object of type "++Item++".",
        description => "Deletes an existing object of type "++Item++".",
        consumes => [?MIMETYPE_DOC],
        produces => [?MIMETYPE_DOC],
        responses => #{
            <<"204">> => #{description => "The object was deleted successfully."},
            <<"400">> => #{description => "The object is invalid."},
            <<"404">> => #{description => "The object does not exist."}
        }
      }
     }.

%% @private called when starting the handling of a request
init(_Transport, Req, [Model]) ->
    State = #state{model=Model, model_state=Model:init()},
    {upgrade, protocol, cowboy_rest, Req, State}.

%% @private Put the rest model in place
rest_init(Req, State) ->
    %% The resource id may be `undefined' in which case, only
    %% `POST' should be accepted (as we define later) and the
    %% model will have to come up with an ID.
    {Id, Req1} = cowboy_req:binding(id, Req),
    Req2 = cowboy_req:set_meta(log_enabled, true, Req1),
    Req3 = cowboy_req:set_meta(start_time, State#state.start, Req2),
    Req4 = cowboy_req:set_meta(metrics_source, State#state.model, Req3),
    {ok, Req4, State#state{resource_id=Id}}.

%% @private called when terminating the handling of a request
rest_terminate(_Req, #state{model=Model, model_state=MState}) ->
    Model:terminate(MState).

%% @private Check if we're configured to be in lockdown mode to see if the
%% service should be available
service_available(Req, State) ->
    {not application:get_env({{appname}}, lockdown, false), Req, State}.

%% @private Allow CRUD stuff standard
allowed_methods(Req, State) ->
    {[<<"POST">>,<<"GET">>,<<"PUT">>,<<"DELETE">>], Req, State}.

%% @private No authorization in place for this demo, but this is it where it goes
is_authorized(Req, State) ->
    {true, Req, State}.

%% @private Does the resource exist at all?
resource_exists(Req, State=#state{resource_id=undefined}) ->
    {false, Req, State};
resource_exists(Req, State=#state{resource_id=Id, model=M, model_state=S}) ->
    case M:read(Id, S) of
        { {ok, Resource}, NewS} ->
            {true, Req, State#state{model_state=NewS, resource=Resource}};
        { {error,not_found}, NewS} ->
            {false, Req, State#state{model_state=NewS}}
    end.

%%%%%%%%%%%%%%%%%%%%%
%%% GET Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%

%% @private
content_types_provided(Req, State) ->
    {[{?MIMETYPE, to_json}], Req, State}.

%% @private
to_json(Req, State=#state{resource=Resource}) when is_map(Resource) ->
    {jsx:encode(Resource), Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% DELETE Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%
%% @private
delete_resource(Req, State=#state{resource_id=Id, model=M, model_state=S}) ->
    case M:delete(Id, S) of
        {true, NewS} -> {true, Req, State#state{model_state=NewS}};
        {_, NewS} -> {false, Req, State#state{model_state=NewS}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT/POST Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private Define content types we accept and handle
content_types_accepted(Req, State) ->
    {[{?MIMETYPE, from_json}], Req, State}.

%% @private Is the operation being attempted conflicting with
%% existing resources
is_conflict(Req, State) ->
    {false, Req, State}.

%% @private Handle the update and figure things out.
from_json(Req, State=#state{resource_id=Id, resource=R, model=M, model_state=S}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Method, Req2} = cowboy_req:method(Req1),
    case Method of
        <<"PUT">> when Id =:= undefined; R =:= undefined ->
            {false, Req2, State};
        _ ->
            try
                Map = jsx:decode(Body, [return_maps]),
                case {Method, M:validate(Map, S)} of
                    {<<"POST">>, {true, S2}} ->
                        {Res, S3} = M:create(Id, Map, S2),
                        {NewRes, Req3} = maybe_expand(Res, Req2),
                        {NewRes, Req3, State#state{model_state=S3}};
                    {<<"PUT">>, {true, S2}} ->
                        {Res, S3} = M:update(Id, Map, S2),
                        {Res, Req2, State#state{model_state=S3}};
                    {_, {false, S2}} ->
                        {false, Req2, State#state{model_state=S2}}
                end
            catch
                error:badarg ->
                    {false, Req2, State}
            end
    end.

%% @private if the resource returned a brand new ID, set the value properly
%% for the location header.
maybe_expand({true, Id}, Req) ->
    {Path, Req2} = cowboy_req:path(Req),
    { {true, [Path, Id]}, Req2};
maybe_expand(Val, Req) ->
    {Val, Req}.
