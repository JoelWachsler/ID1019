-module (recursion).

-compile (export_all).

product(0, _) ->
  0;
product(M, N) ->
  N + product(M - 1, N).

exp(_, 0) ->
    1;
exp(X, Y) ->
  product(X, exp(X, Y - 1)).

expIm(X, 1) ->
  X;
expIm(X, N) ->
  case N rem 2 of
    0 -> expIm(X, N div 2) * expIm(X, N div 2);
    _ -> expIm(X, N - 1) * X
  end.
