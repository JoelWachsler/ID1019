-module (isort).

-compile(export_all).

insert(Element, []) ->
  [Element];
insert(Element, [H|T]) ->
  case H >= Element of
    true -> [Element, H | T];
    false -> [H | insert(Element, T)]
  end.


isort(L) ->
  isort(L, []).

isort([], S) ->
  S;
isort([H|T], S) ->
  isort(T, insert(H, S)).
