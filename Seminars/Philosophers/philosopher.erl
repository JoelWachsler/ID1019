-module (philosopher).

-compile(export_all).

start(Hungry, Left, Right, Name, Ctrl) ->
  spawn_link(fun() -> philosopher(Hungry, Left, Right, Name, Ctrl) end).

return_chop([]) -> [];
return_chop([H | T]) ->
  chopstick:return(H),
  return_chop(T).

get_chop(_, _, Name, [_, _]) ->
  io:format("~s got both chopsticks~n", [Name]);
get_chop(Left, Right, Name, Holding) ->
  receive
    {Left, ok} ->
      io:format("~s got the left chopstick~n", [Name]),
      get_chop(Left, Right, Name, [Left | Holding]);
    {Right, ok} ->
      io:format("~s got the right chopstick~n", [Name]),
      get_chop(Left, Right, Name, [Right | Holding])
  after 1000 ->
    io:format("~s failed to get both chopsticks - returning~n", [Name]),
    return_chop(Holding),
    io:format("~s is sleeping a bit and will try to get chopsticks in a while~n", [Name]),
    sleep(100, 300),
    io:format("~s is trying to get chopsticks again~n", [Name]),
    get_chop(Left, Right, Name, [])
  end.

philosopher(0, _, _, _, Ctrl) ->
  Ctrl ! done;
philosopher(Hungry, Left, Right, Name, Ctrl) ->
  io:format("~s is sleeping with ~B times left to eat~n", [Name, Hungry]),
  sleep(100, 300),
  io:format("~s is requesting the left chopstick~n", [Name]),
  chopstick:request(self(), Left),
  io:format("~s is requesting the right chopstick~n", [Name]),
  chopstick:request(self(), Right),

  get_chop(Left, Right, Name, []),

  io:format("~s is eating~n", [Name]),
  eat(100, 300),
  io:format("~s is returning the left chopstick~n", [Name]),
  chopstick:return(Left),
  io:format("~s is returning the right chopstick~n", [Name]),
  chopstick:return(Right),
  philosopher(Hungry - 1, Left, Right, Name, Ctrl).

sleep(T, D) ->
  timer:sleep(T + rand:uniform(D)).

eat(T, D) ->
  timer:sleep(T + rand:uniform(D)).

tests(Console) ->
  start(5, chopstick:start(), chopstick:start(), "Arendt", Console).
