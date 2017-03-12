-module (test).

-compile(export_all).

double(N) ->
  N * 2.

tempConv(F) ->
  (F-32)/1.8.

rectArea(X, Y) ->
  X*Y.

sqArea(X) ->
  rectArea(X, X).

circleArea(R) ->
  math:pi() * math:pow(R, 2).
