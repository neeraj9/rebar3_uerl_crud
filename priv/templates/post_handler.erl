%%% @doc a generic CRUD handler based on cowboy's rest framework.
-module({{name}}_handler).

%% Docs callbacks
-export([paths/1, paths/2]).
%% cowboy_rest callbacks
-export([init/3, rest_init/2, rest_terminate/2, service_available/2,
         allowed_methods/2, is_authorized/2,
         content_types_accepted/2, from_json/2]).
%% Don't warn on known optional callbacks
-ignore_xref([paths/1, paths/2, init/3, rest_init/2, rest_terminate/2,
              service_available/2, allowed_methods/2,
              is_authorized/2, content_types_provided/2, to_json/2,
              delete_resource/2, content_types_accepted/2, from_json/2
              ]).

-record(state, {model :: module(),
                start = erlang:monotonic_time(milli_seconds),
                model_state :: term(),
                resource :: map() | undefined}).

-define(MIMETYPE, {<<"application">>, <<"json">>, '*'}).
-define(MIMETYPE_DOC, "application/json").

paths(Item) ->
    paths(Item, default_spec(Item)).

paths(Item, Spec) when is_binary(Item) ->
    paths(unicode:characters_to_list(Item), Spec);
paths(Item, Spec) when is_atom(Item) ->
    paths(atom_to_list(Item), Spec);
paths(Item, Spec) when is_list(Item) ->
    #{post => #{
        parameters => [Spec],
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
      }
     }.

default_spec(Item) ->
    %% See http://swagger.io/specification/#schemaObject
    #{name => Item,
      in => body,
      description => "Object of type "++Item++".",
      required => true,
      schema => #{
        type => "object",
        properties => #{
          spec => #{type => "string"}
         }
       }}.

%% @private called when starting the handling of a request
init(_Transport, Req, [Model]) ->
    State = #state{model=Model, model_state=Model:init()},
    {upgrade, protocol, cowboy_rest, Req, State}.

%% @private Put the rest model in place
rest_init(Req, State) ->
    Req2 = cowboy_req:set_meta(log_enabled, true, Req),
    Req3 = cowboy_req:set_meta(start_time, State#state.start, Req2),
    Req4 = cowboy_req:set_meta(metrics_source, State#state.model, Req3),
    {ok, Req4, State}.

%% @private called when terminating the handling of a request
rest_terminate(_Req, #state{model=Model, model_state=MState}) ->
    Model:terminate(MState).

%% @private Check if we're configured to be in lockdown mode to see if the
%% service should be available
service_available(Req, State) ->
    {not application:get_env({{appname}}, lockdown, false), Req, State}.

%% @private Allow CRUD stuff standard
allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%% @private No authorization in place for this demo, but this is it where it goes
is_authorized(Req, State) ->
    {true, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PUT/POST Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @private Define content types we accept and handle
content_types_accepted(Req, State) ->
    {[{?MIMETYPE, from_json}], Req, State}.

%% @private Handle the update and figure things out.
from_json(Req, State=#state{resource=R, model=M, model_state=S}) ->
    {ok, Body, Req1} = cowboy_req:body(Req),
    {Method, Req2} = cowboy_req:method(Req1),
    case Method of
        <<"POST">> ->
            try
                Map = jiffy:decode(Body, [return_maps]),
		            % TODO do something with the body
		            % TODO maybe the model state changes due to some operation
		            S3 = S,
                % case1: created resource but new Id and no body
                {true, Req2, State#state{model_state=S3}}
		            % case2: Id created as part of POST
                %  {Path, Req2} = cowboy_req:path(Req),
                %  { {true, [Path, trail_slash(Path), Id]}, Req2, State}
		            % case3: Id updated as part of PUT
		            %  {Id, Req2, State}
            catch
                error:badarg ->
                    {false, Req2, State}
            end;
        _ -> {false, Req2, State#state{model_state=S}}
    end.

maybe_expand(Val, Req) ->
    {Val, Req}.

trail_slash(Path) ->
    case binary:last(Path) of
        $/ -> "";
        _ -> "/"
    end.
