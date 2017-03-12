-module(moves).

-export([move/2, tests/0]).

% For example, single({one,1},{[a,b],[],[]}) returns {[a],[b],[]}.
% single should be used later in this assignment whenever a move is to
% be performed on a state
single({_, 0}, {Main, One, Two}) ->
  {Main, One, Two};
single({one, N}, {Main, One, Two}) ->
  case N > 0 of
    true ->
      {list:take(Main, length(Main) - N), list:append(list:drop(Main, length(Main) - N), One), Two};
    false ->
      {list:append(Main, list:take(One, -N)), list:drop(One, -N), Two}
  end;
single({two, N}, {Main, One, Two}) ->
  case N > 0 of
    true ->
      {list:take(Main, length(Main) - N), One, list:append(list:drop(Main, length(Main) - N), Two)};
    false ->
      {list:append(Main, list:take(Two, -N)), One, list:drop(Two, -N)}
  end.

move([], State) -> [State];
move([H | T], State) ->
  [State | move(T, single(H, State))].

tests() ->
  [
    single({one,1},{[a,b],[],[]}) =:= {[a],[b],[]},
    single({one,-1},{[a],[b],[]}) =:= {[a,b],[],[]},
    single({two,-1},{[a],[b],[c]}) =:= {[a,c],[b],[]},
    move([{one,1},{two,1},{one,-1}], {[a,b],[],[]}) =:= [{[a,b],[],[]}, {[a],[b],[]}, {[],[b],[a]}, {[b],[],[a]}]
  ].
