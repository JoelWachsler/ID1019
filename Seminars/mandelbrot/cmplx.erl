-module(cmplx).

-compile([export_all]).

new(X, Y) ->
  {X, Y}.

add({X1, Y1}, {X2, Y2}) ->
  {X1 + X2, Y1 + Y2}.

sqr({X, Y}) ->
  {X*X - Y*Y, 2*X*Y}.

cabs({X, Y}) ->
  math:sqrt(X*X + Y*Y).
