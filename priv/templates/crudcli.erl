%%% @doc Simple client based on `httpc' to deal with CRUD calls over HTTP.
%%% Assumes JSON.
-module(crudcli).
-export([create/3, create_valid/3, create_invalid/3, recreate_valid/3, recreate_invalid/3,
         read/2, read_exists/2, read_unknown/2,
         update/3, update_valid/3, update_invalid/3, update_unknown/3,
         delete/2, delete_exists/2, delete_unknown/2]).

-define(MIME, "application/json").

create(Host, Path, Map) ->
    handle_res(httpc:request(post, {Host++Path, [], ?MIME, jiffy:encode(Map)},
                             [], [{body_format, binary}])).

%% alias for nicer reporting
create_valid(Host, Path, Map) -> create(Host, Path, Map).
create_invalid(Host, Path, Map) -> create(Host, Path, Map).
recreate_valid(Host, Path, Map) -> create(Host, Path, Map).
recreate_invalid(Host, Path, Map) -> create(Host, Path, Map).


read(Host, Path) ->
    handle_res(httpc:request(get, {Host++Path, []},
                             [], [{body_format, binary}])).

%% alias for nicer reporting
read_exists(Host, Path) -> read(Host, Path).
read_unknown(Host, Path) -> read(Host, Path).

update(Host, Path, Map) ->
    handle_res(httpc:request(put, {Host++Path, [], ?MIME, jiffy:encode(Map)},
                             [], [{body_format, binary}])).

%% alias for nicer reporting
update_valid(Host, Path, Map) -> update(Host, Path, Map).
update_invalid(Host, Path, Map) -> update(Host, Path, Map).
update_unknown(Host, Path, Map) -> update(Host, Path, Map).

delete(Host, Path) ->
    handle_res(httpc:request(delete, {Host++Path, []},
                             [], [{body_format, binary}])).

%% alias for nicer reporting
delete_exists(Host, Path) -> delete(Host, Path).
delete_unknown(Host, Path) -> delete(Host, Path).


handle_res({ok, {{_HTTP, Code, _Status}, Headers, <<>>}}) ->
    {Code, Headers, undefined};
handle_res({ok, {{_HTTP, Code, _Status}, Headers, Body}}) ->
    DecodedBody = try
                      jiffy:decode(Body, [return_maps])
                  catch
                      _:_ ->
                          io:format("Body: ~p~n", [Body]),
                          invalid_body
                  end,
    {Code, Headers, DecodedBody}.

