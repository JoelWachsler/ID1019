-module (qsort).

-compile(export_all).

qsplit(_, [], L, R) ->
  {L, R};
qsplit(P, [H|T], L, R) ->
  case P >= H of
    true  -> qsplit(P, T, [H|L], R);
    false -> qsplit(P, T, L, [H|R])
  end.

qsort([]) ->
  [];
qsort([E]) ->
  [E];
qsort([P|T]) ->
  {L, R} = qsplit(P, T, [], []),
  SmallSorted = qsort(L),
  LargeSorted = qsort(R),
  SmallSorted ++ [P] ++ LargeSorted.
