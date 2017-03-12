-module(shunt).

-compile(export_all).

% split([a,b,c],a) = {[],[b,c]}
% split([a,b,c],b) = {[a],[c]}
split(List, Y) ->
  Pos = list:position(List, Y),
  {list:take(List, Pos - 1), list:drop(List, Pos)}.

find([], []) -> [];
find([X | Xs], [Y | Ys]) ->
  {Hs, Ts} = split([X | Xs], Y),
  Commands = [{one, 1 + length(Ts)}, {two, length(Hs)}, {one, -(1 + length(Ts))}, {two, -length(Hs)}],
  Moves = moves:move(Commands, {[X | Xs], [], []}),
  [{[_ | LastState], _, _}] = list:drop(Moves, length(Moves) - 1),
  list:append(Commands, find(LastState, Ys)).

few([], []) -> [];
few([X | Xs], [X | Ys]) ->
  few(Xs, Ys);
few([X | Xs], [Y | Ys]) ->
  {Hs, Ts} = split([X | Xs], Y),
  Commands = [{one, 1 + length(Ts)}, {two, length(Hs)}, {one, -(1 + length(Ts))}, {two, -length(Hs)}],
  Moves = moves:move(Commands, {[X | Xs], [], []}),
  [{[_ | LastState], _, _}] = list:drop(Moves, length(Moves) - 1),
  list:append(Commands, few(LastState, Ys)).

% 1. Replace {one,N} directly followed by {one,M} with {one,N+M}.
% 2. Replace {two,N} directly followed by {two,M} with {two,N+M}.
% 3. Remove {one,0}.
% 4. Remove {two,0}.
rules([{Track, N}, {Track, M} | T]) ->
  [{Track, N + M} | rules(T)];
rules([{_, 0} | T]) -> rules(T);
rules([H | T]) -> [H | rules(T)];
rules([]) -> [].

compress(Ms) ->
  Ns = rules(Ms),
  if Ns == Ms -> Ms;
     true -> compress(Ns)
  end.

% Ms as the wagons on “main”
% Os as the wagons on “one”
% Ts as the wagons on “two”
% Ys as the desired train
%fewer(Ms, Os, Ts, [Y | Ys]) ->
  


tests() ->
  [
    split([a,b,c,d,e], c) =:= {[a,b],[d,e]},
    split([a,b,c],a) =:= {[],[b,c]},
    split([a,b,c],b) =:= {[a],[c]},
    find([a,b], [b,a]) =:= [{one,1},{two,1},{one,-1},{two,-1}, {one,1},{two,0},{one,-1},{two,0}],
    few([c,a,b], [c,b,a]) =:= [{one,1},{two,1},{one,-1},{two,-1}],
    compress([{two,-1},{one,1},{one,-1},{two,1}]) =:= []
  ].
