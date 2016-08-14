# {{name}} #

An application for CRUD handling.

## Adding a Handler ##

    $ rebar3 new crud-handler my_resource

Then add the `my_resource_model` entry to the list of results in `src/router.erl`

The commands in [Testing](#testing) should then pass.

## Testing ##

    $ rebar3 check

## Building ##

    $ rebar3 release
    $ ./_build/default/bin/{{name}}


## Development shell ##

    $ rebar3 shell

## API Documentation ##

Available through Swagger at [http://localhost:8001/api-docs/](http://localhost:8001/api-docs/index.html)

## Reference Documentation ##

Reference documentation for the source code can be generated in `doc/` by
calling:

    $ rebar3 edoc

## Metrics ##

First, start the system.

Query the list of available metrics:

    $ curl http://localhost:5565/_metrics

Query a specific metric:

    $ curl http://localhost:5565/_metrics/name

Query Erlang VM metrics:

    $ curl http://localhost:5565/_memory

Query Erlang VM stats:

    $ curl http://localhost:5565/_statistics

Query Erlang VM information:

    $ curl http://localhost:5565/_system

