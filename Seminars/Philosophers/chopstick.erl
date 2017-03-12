-module (chopstick).

-compile(export_all).

start() ->
  spawn_link(fun() -> available() end).

request(Sender, Stick) ->
  Stick ! {Sender, request}.

return(Stick) ->
  Stick ! return.

available() ->
  receive
    {Pid, request} ->
      Pid ! {self(), ok},
      gone();
    quit ->
      ok
  end.

gone() ->
  receive
    return ->
      available();
    quit ->
      ok
  end.
