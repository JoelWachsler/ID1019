-module (msort).

-compile(export_all).

msplit([]) ->
  [];
msplit(List) ->
  msplit(List, [], []).

msplit([], L, R) ->
  {L, R};
msplit([H1, H2 | T], L, R) ->
  msplit(T, [H1 | L], [H2 | R]);
msplit([H | _], L, R) ->
  {[H | L], R}.

merge([], []) ->
  [];
merge([], [H|T]) ->
  [H|merge([], T)];
merge([H|T], []) ->
  [H|merge(T, [])];
merge([HL|L], [HR|R]) ->
  case HL < HR of
    true  -> [HL|merge(L, [HR|R])];
    false -> [HR|merge([HL|L], R)]
  end.

msort([]) ->
  [];
msort([E]) ->
  [E];
msort(List) ->
  {L, R} = msplit(List),
  merge(msort(L), msort(R)).
