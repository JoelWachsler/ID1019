-module (list).

-compile(export_all).

nth(N, [H|R]) ->
  case N of
    0 -> H;
    _ -> nth(N - 1, R)
  end.

number(L) ->
  number(L, 0).

number([], C) ->
  C;
number([_|R], C) ->
  number(R, C + 1).

sum(L) ->
  sum(L, 0).

sum([], S) ->
  S;
sum([H|T], S) ->
  sum(T, H + S).

duplicate([]) ->
  [];
duplicate([H|T]) ->
  [H, H | duplicate(T)].

unique([]) ->
  [];
unique([H|T]) ->
  [H | unique([X ||  X <- T, X /= H])].

reverse(L) ->
  reverse(L, []).

reverse([], Acc) ->
  Acc;
reverse([H|T], Acc) ->
  reverse(T, [H|Acc]).

packTest() ->
  pack([a,a,b,c,b,a,c]).

pack([]) ->
  [];
pack([H|T]) ->
  [[X || X <- [H|T], X == H] | pack([X || X <- T, X /= H])].
