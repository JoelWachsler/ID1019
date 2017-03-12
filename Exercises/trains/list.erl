-module(list).

-export([take/2, drop/2, append/2, member/2, position/2, tests/0]).

% returns the list containing the first N elements of Xs.
take(Xs, N) -> take(Xs, N, []).

take([], _, Acc) -> lists:reverse(Acc);
take(_, 0, Acc) -> lists:reverse(Acc);
take([H | T], N, Acc) ->
  take(T, N - 1, [H | Acc]).

% returns the list Xs without its first N elements.
drop([], _) -> [];
drop(Xs, 0) -> Xs;
drop([_ | T], N) ->
  drop(T, N - 1).

% returns the list Xs with the elements of Ys appended.
append(Xs,Ys) ->
  appendAcc(Ys, lists:reverse(Xs)).
appendAcc([], Acc) -> lists:reverse(Acc);
appendAcc([H | T], Acc) ->
  appendAcc(T, [H | Acc]).

% tests whether Y is an element of Xs.
member([], _) -> false;
member([Y | _], Y) -> true;
member([_ | T], Y) -> member(T, Y).

% returns the first position of Y in the list Xs. You can
% assume that Y is an element of Xs
position(Xs, Y) -> position(Xs, Y, 1).
position([Y | _], Y, N) -> N;
position([_ | T], Y, N) -> position(T, Y, N + 1).

tests() ->
  [
    take([a,b,c,d], 3) =:= [a,b,c],
    drop([a,b,c,d], 3) =:= [d],
    append([a,b], [c, d]) =:= [a,b,c,d],
    member([a,b,c,d], c) =:= true,
    member([a,b,c,d], p) =:= false,
    position([a,b,c,d], c) =:= 3
  ].
