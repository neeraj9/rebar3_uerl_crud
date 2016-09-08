rebar3_uerl_crud
=====

Erlang Microkernel CRUD templates plugin

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_crud, {git, "https://github.com/neeraj9/rebar3_uerl_crud.git", {branch, "master"}}}
    ]}.

Then call:

    $ rebar3 new crud-project -f
    $ less README.md
