-module(huffman).

-compile(export_all).

sample() -> "the quick brown fox jumps over the lazy dog
this is a sample text that we will use when we build
up a table we will only handle lower case letters and
no punctuation symbols the frequency will of course not
represent english but it is probably not that far off".

text() -> "this is something that we should encode".

huffman_benchmark() ->
  lists:map(
    fun(E) ->
      {Time, _} = timer:tc(huffman, huffman_code, [read("kallocain.txt", E)]),
      {E, Time}
    end,
    [trunc(math:pow(2, N)) || N <- lists:seq(5, 19)]).

huffman_code(Text) ->
  Tree = tree(Text),
  Encode = encode_table(Tree),
  Decode = decode_table(Tree),
  Seq = encode(Text, Encode),
  Text = decode(Seq, Decode).

test() ->
  Sample = sample(),
  Tree = tree(Sample),
  Encode = encode_table(Tree),
  Decode = decode_table(Tree),
  Text = text(),
  Seq = encode(Text, Encode),
  Text = decode(Seq, Decode).

% node form: {node, Value, Child1, Child2}
tree(Text) ->
  Freq = freq(Text),
  NodeList = to_node_list(Freq),
  create_tree(NodeList).

create_tree([]) -> [];
create_tree([{K, V, L, R}]) when L =:= nil ->
  {nil, V, nil, {K, V, L, R}};
create_tree([E]) -> E;
create_tree([{K1, V1, L1, R1}, {K2, V2, L2, R2}]) ->
  create_tree([{nil, V1 + V2, {K2, V2, L2, R2}, {K1, V1, L1, R1}}]);
create_tree([{K1, V1, L1, R1}, {K2, V2, L2, R2} | T]) ->
  create_tree(freq_sort([{nil, V1 + V2, {K2, V2, L2, R2}, {K1, V1, L1, R1}} | T])).

encode_table(Tree) ->
  Table = table(Tree),
  lists:foldl(
    fun({Key, Value}, Dict) ->
        dict:store(Key, Value, Dict) end,
    dict:new(),
    Table).

decode_table(Tree) ->
  Table = table(Tree),
  lists:foldl(
    fun({Value, Key}, Dict) ->
        dict:store(Key, Value, Dict) end,
    dict:new(),
    Table).

table([]) -> [];
table(Tree) ->
  lists:flatten(table(Tree, [])).

table({Key, _, _, _}, Path) when Key =/= nil ->
  [{Key, Path}];
table({Key, _, nil, RightChild}, Path) when Key =:= nil ->
  [table(RightChild, [0 | Path])];
table({Key, _, LeftChild, RightChild}, Path) when Key =:= nil ->
  [table(LeftChild, [0 | Path]),
  table(RightChild, [1 | Path])].

encode(Text, Dict) ->
  lists:foldr(
    fun(C, Acc) ->
      dict:fetch(C, Dict) ++ Acc
    end,
    [],
    Text).

decode(Seq, Dict) ->
  {_, Text} = lists:foldr(
    fun(C, {Bits, Acc}) ->
      case dict:find([C | Bits], Dict) of
        {ok, Value} -> {[], [Value | Acc]};
        error -> {[C | Bits], Acc}
      end
    end,
    {[], []},
    Seq),
  Text.

to_node_list(Freq) ->
  [{K, V, nil, nil} || {K, V} <- Freq].

freq_sort(Freq) ->
  lists:keysort(2, Freq).

freq(Text) ->
  Dict = lists:foldr(
    fun(Char, Freq) ->
      dict:update_counter(Char, 1, Freq)
    end,
    dict:new(),
    Text),
  List = dict:to_list(Dict),
  freq_sort(List).

%% -- DEBUGGING --
freq_test() ->
  freq("abcacc") =:= [{98,1},{97,2},{99,3}].

tree_test() ->
  Freq = freq("abcacc"),
  NodeList = to_node_list(Freq),
  tree(NodeList) =:= [{nil,6,
      {nil,3,{98,1,nil,nil},{97,2,nil,nil}},
      {99,3,nil,nil}}].

test_encode_table() ->
  Freq = freq("abcacc"),
  NodeList = to_node_list(Freq),
  Tree = tree(NodeList),
  encode_table(Tree).

test_encode() ->
  Text = "this is somethin...",
  Tree = tree(Text),
  Table = encode_table(Tree),
  {Table, encode(Text, Table)}.

test_decode() ->
  Text = "this is somethin...",
  Tree = tree(Text),
  EncTable = encode_table(Tree),
  Seq = encode(Text, EncTable),
  DecTable = decode_table(Tree),
  decode(Seq, DecTable).

print_freq(L) ->
  [{<<X>>, Y} || {X, Y} <- freq(L)].

%% -- Testing with text from file --
read(File, N) ->
  {ok, Fd} = file:open(File, [read, binary]),
  {ok, Binary} = file:read(Fd, N),
  file:close(Fd),
  case unicode:characters_to_list(Binary, utf8) of
    {incomplete, List, _} ->
      List;
    List ->
      List
  end.
