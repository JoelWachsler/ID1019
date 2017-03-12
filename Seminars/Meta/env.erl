-module(env).

-compile(export_all).

new() -> [].

add(Id, Str, Env) ->
  [{Id, Str} | Env].

lookup(_, []) -> false;
lookup(Id, [{Id, _}=Entry | _]) -> Entry;
lookup(Id, [_ | Env]) -> lookup(Id, Env).

lookupTest() ->
  lookup(foo, add(foo, 42, new())).
