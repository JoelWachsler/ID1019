-module(eager).

-compile(export_all).

eval_expr({atm, Id}, _) ->
  {ok, Id};
eval_expr({var, Id}, Env) ->
  case env:lookup(Id, Env) of
    false ->
      error;
    {_, Val} ->
      {ok, Val}
  end;
eval_expr({cons, Head, Tail}, Env) ->
  case eval_expr(Head, Env) of
    error ->
      error;
    {ok, Item} ->
      case eval_expr(Tail, Env) of
        error ->
          error;
        {ok, UnpackedTail} ->
          {ok, {Item, UnpackedTail}}
      end
    end.

eval_match(ignore, _, Env) ->
  {ok, Env};
eval_match({atm, Id}, Id, Env) ->
  {ok, Env};
eval_match({var, Id}, Str, Env) ->
  case env:lookup(Id, Env) of
    false ->
      {ok, env:add(Id, Str, Env)};
    {Id, Str} ->
      {ok, Env};
    {Id, _} ->
      fail
  end;

eval_match({cons, Id1, Id2}, {Str1, Str2}, Env) ->
  case eval_match(Id1, Str1, Env) of
    fail ->
      fail;
    {ok, NewEnv} ->
      eval_match(Id2, Str2, NewEnv)
  end;
eval_match(_, _, _) ->
  fail.

eval_seq([Exp], Env) ->
  eval_expr(Exp, Env);
eval_seq([{match, Ptr, Exp} | Seq], Env) ->
  case eval_expr(Exp, Env) of
    error ->
      eval_seq(Seq, eval_match(Ptr, Exp, Env));
    {ok, Str} ->
      case eval_match(Ptr, Str, Env) of
        fail ->
          error;
        {ok, NewEnv} ->
          eval_seq(Seq, NewEnv)
      end
  end.

eval(Seq) ->
  eval_seq(Seq, env:new()).

%% ---------- DEBUG TESTS ---------- %%

eval_test() ->
  [
    eval_expr({atm, a}, []) =:= {ok, a},
    eval_expr({var, x}, [{x, a}]) =:= {ok, a},
    eval_expr({var, x}, []) =:= error,
    eval_expr({cons, {var, x}, {atm, []}}, [{x, a}]) =:= {ok, {a, []}}
  ].

match_test() ->
  [
    eval_match({atm, a}, a, []) =:= {ok, []},
    eval_match({var, x}, a, []) =:= {ok, [{x, a}]},
    eval_match({var, x}, a, [{x, a}]) =:= {ok, [{x, a}]},
    eval_match({var, x}, a, [{x, b}]) =:= fail
  ].

cons_test() ->
  [
    eval_match({var, x}, a, []) =:= {ok, [{x, a}]},
    eval_match({cons, {var, x}, {var, x}}, {a, a}, []) =:= {ok, [{x, a}]},
    eval_match({cons, {var, x}, {var, x}}, {a, b}, []) =:= fail
  ].

seq_test() ->
  Seq = [
         {match, {var, x}, {atm, a}},
         {match, {var, y}, {cons, {var, x}, {atm, b}}},
         {match, {cons, ignore, {var, z}}, {var, y}},
         {var, z}
        ],
  eval(Seq).
