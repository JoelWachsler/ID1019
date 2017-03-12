-module (bin).

-compile(export_all).

bin(<<>>) -> [];
bin(<<B:1, Rest/bitstring>>) ->
  [B | bin(Rest)];
bin(N) -> bin(<<N/integer>>).
