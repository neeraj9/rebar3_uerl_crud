rebar3_crud
=====

CRUD templates plugin

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_crud, {git, "https://bitbucket.org/ferd/rebar3_crud.git", {branch, "master"}}}
    ]}.

Then call:

    $ rebar3 new crud-project -f
    $ rebar3 new crud-handler my_resource
