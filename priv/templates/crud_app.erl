%%% @private
%%% OTP application handler in charge of putting everything
%%% in place for the handlers and servers to boot properly.
%%% @end
-module({{name}}_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Application callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% This function is called whenever an application is started using
%% `application:start/[1,2]', and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
start(_StartType, _StartArgs) ->
    Specs = router:specs() ++ [cowboy_swagger_handler],
    Trails = trails:trails(Specs),
    trails:store(Trails),
    Dispatch = trails:single_host_compile(Trails),
    {ok, Port} = application:get_env({{name}}, port),
    cowboy:start_http({{name}}_listener, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]},
                       {onresponse, fun log_utils:req_log/4}]),
    {{name}}_sup:start_link(router:specs()).

%% @private
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
stop(_State) ->
    cowboy:stop_listener({{name}}_listener),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%


