%%% @doc a generic CRUD handler based on cowboy's rest framework.
-module({{name}}_handler).

%% Docs callbacks
-export([paths/1, paths/2]).
%% cowboy_rest callbacks
-export([init/3, rest_init/2, rest_terminate/2, service_available/2,
         allowed_methods/2, is_authorized/2,
         content_types_provided/2, to_json/2, to_html/2
         ]).
%% Don't warn on known optional callbacks
-ignore_xref([paths/1, paths/2, init/3, rest_init/2, rest_terminate/2,
              service_available/2, allowed_methods/2,
              is_authorized/2, content_types_provided/2, to_json/2
              ]).

-record(state, {model :: module(),
                start = erlang:monotonic_time(milli_seconds),
                model_state :: term(),
                resource :: map() | undefined}).

-define(MIMETYPE, {<<"application">>, <<"json">>, '*'}).
-define(MIMETYPE_HTML, {<<"text">>, <<"html">>, '*'}).
-define(MIMETYPE_DOC, "application/json").

paths(Item) ->
    paths(Item, default_spec(Item)).

paths(Item, Spec) when is_binary(Item) ->
    paths(unicode:characters_to_list(Item), Spec);
paths(Item, Spec) when is_atom(Item) ->
    paths(atom_to_list(Item), Spec);
paths(Item, _Spec) when is_list(Item) ->
    #{get => #{
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

%% @private Allow only read out of the CRUD
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

%% @private No authorization in place for this demo, but this is it where it goes
is_authorized(Req, State) ->
    {true, Req, State}.


%%%%%%%%%%%%%%%%%%%%%
%%% GET Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%

%% @private
content_types_provided(Req, State) ->
    {[{?MIMETYPE, to_json},
      {?MIMETYPE_HTML, to_html}], Req, State}.

%% @private
to_json(Req, State) ->
    T1 = <<"{ \"msg\" : \"It is alive @ ">>,
    T2 = <<" when called by ">>,
    T3 = <<"\"}">>,
    Date = list_to_binary(httpd_util:rfc1123_date()),

    {{IP, _Port}, Req2} = cowboy_req:peer(Req),
    % do we really need to resolve to hostname?
    Hostname = resolv_hostname_from_ip(IP),

    Response = <<T1/binary, Date/binary, T2/binary, Hostname/binary, T3/binary>>,
    lager:error("sending response=~w", [Response]),
    {Response, Req, State}.

%% @private
to_html(Req, State) ->
  T1 = <<"<html><body>It is alive @ ">>,
  T2 = <<" when called by ">>,
  T3 = <<"</body></html>">>,
  Date = list_to_binary(httpd_util:rfc1123_date()),

  {{IP, _Port}, Req2} = cowboy_req:peer(Req),
  % do we really need to resolve to hostname?
  Hostname = resolv_hostname_from_ip(IP),

  Response = <<T1/binary, Date/binary, T2/binary, Hostname/binary, T3/binary>>,
  lager:error("sending response=~w", [Response]),
  {Response, Req, State}.

-include_lib("kernel/include/inet.hrl").

resolv_hostname_from_ip(IP) ->
  case inet:gethostbyaddr(IP) of
    {ok, HostEnt} ->
      HostnameString = HostEnt#hostent.h_name;
    _ ->
      HostnameString = inet_parse:ntoa(IP)
  end,
  list_to_binary(HostnameString).
