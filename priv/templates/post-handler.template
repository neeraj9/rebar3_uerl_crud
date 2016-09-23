{description, "Set up a project for POST only HTTP interfaces."}.
{variables, [
  {name, "postapi", "Name of the resource"},
  {appname, "crud", "Name of the parent app."}
]}.

{dir, "handlers/"}.
{template, "post_handler.erl", "handlers/{{name}}_handler.erl"}.

{dir, "models/"}.
{template, "post_model.erl", "models/{{name}}_model.erl"}.

% test is not finalized
%{dir, "test/"}.
%{file, "crudcli.erl", "test/crudcli.erl"}.
%{template, "model_post.erl", "test/model_{{name}}.erl"}.
%{template, "prop_crud.erl", "test/prop_{{name}}.erl"}.
