-module(tree).

-compile([export_all]).

insertf(Key, Value, nil) ->
  {leaf, Key, Value};
insertf(K, V, {leaf, K1, _}=L) ->
  if
    K =< K1 ->
      {two, K, {leaf, K, V}, L};
    true ->
      {two, K1, L, {leaf, K, V}}
  end;
insertf(K, V, {two, K1, {leaf, K1, _}=L1,
                        {leaf, K2, _}=L2}) ->
  if
    K =< K1 ->
      {three, K, K1, {leaf, K, V}, L1, L2};
    K =< K2 ->
      {three, K1, K, L1, {leaf, K, V}, L2};
    true ->
      {three, K1, K2, L1, L2, {leaf, K, V}}
  end;

insertf(K, V, {three, K1, K2, {leaf, K1, _}=L1,
                              {leaf, K2, _}=L2,
                              {leaf, K3, _}=L3}) ->
  if
    K =< K1 ->
      {four, K, K1, K2, {leaf, K, V}, L1, L2, L3};
    K =< K2 ->
      {four, K1, K, K2, L1, {leaf, K, V}, L2, L3};
    K =< K3 ->
      {four, K1, K2, K, L1, L2, {leaf, K, V}, L3};
    true ->
      {four, K1, K2, K3, L1, L2, L3, {leaf, K, V}}
  end;

insertf(K, V, {two, K1, Left, Right}) ->
  if
    K =< K1 ->
      case insertf(K, V, Left) of
        {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
          {three, Q2, K1,
            {two, Q1, T1, T2},
            {two, Q3, T3, T4},
            Right
          };
        Updated ->
          {two, K1, Updated, Right}
      end;
    true ->
      case insertf(K, V, Right) of
        {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
          {three, K1, Q2,
            Left,
            {two, Q1, T1, T2},
            {two, Q3, T3, T4}
          };
        Updated ->
          {two, K1, Left, Updated}
      end
  end;

insertf(K, V, {three, K1, K2, Left, Middle, Right}) ->
  if
    K =< K1 ->
      case insertf(K, V, Left) of
        {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
          {four, Q2, K1, K2, {two, Q1, T1, T2}, {two, Q3, T3, T4}, Middle, Right};
        Updated ->
          {three, K1, K2, Updated, Middle, Right}
      end;
    K =< K2 ->
      case insertf(K, V, Middle) of
        {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
          {four, K1, Q2, K2, Left, {two, Q1, T1, T2}, {two, Q3, T3, T4}, Right};
        Updated ->
          {three, K1, K2, Left, Updated, Right}
      end;
    true ->
      case insertf(K, V, Right) of
        {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
          {four, K1, K2, Q2, Left, Middle, {two, Q1, T1, T2}, {two, Q3, T3, T4}};
        Updated ->
          {three, K1, K2, Left, Middle, Updated}
      end
  end.

insert(K, V, Root) ->
  case insertf(K, V, Root) of
    {four, Q1, Q2, Q3, T1, T2, T3, T4} ->
      {two, Q2, T1, {two, Q1, T1, T2}, {two, Q3, T3, T4}};
    Updated ->
      Updated
  end.

tests() ->
  [
    %% ONE NODE TEST %%
    insertf(0, aaa, nil) == {leaf,0,aaa},

    insertf(2, aaa, {leaf, 1, bbb}) == {two,1,{leaf,1,bbb},{leaf,2,aaa}},

    %% THRRE NODE TEST %%
    insertf(1, aaa, {two, 2, {leaf, 2, bbb}, {leaf, 3, ccc}}) ==
    {three, 1, 2, {leaf, 1, aaa}, {leaf, 2, bbb}, {leaf, 3, ccc}},

    insertf(2, aaa, {two, 1, {leaf, 1, bbb}, {leaf, 3, ccc}}) ==
    {three, 1, 2, {leaf, 1, bbb}, {leaf, 2, aaa}, {leaf, 3, ccc}},

    insertf(3, aaa, {two, 1, {leaf, 1, bbb}, {leaf, 2, ccc}}) ==
    {three, 1, 2, {leaf, 1, bbb}, {leaf, 2, ccc}, {leaf, 3, aaa}},

    %% FOUR NODE TEST %%
    insertf(1, aaa, insertf(2, bbb, insertf(3, ccc, insertf(4, ddd, nil)))) ==
    {four, 1, 2, 3, {leaf, 1, aaa}, {leaf, 2, bbb}, {leaf, 3, ccc}, {leaf, 4, ddd}},

    insertf(2, aaa, insertf(1, bbb, insertf(3, ccc, insertf(4, ddd, nil)))) ==
    {four, 1, 2, 3, {leaf, 1, bbb}, {leaf, 2, aaa}, {leaf, 3, ccc}, {leaf, 4, ddd}},

    insertf(3, aaa, insertf(1, bbb, insertf(2, ccc, insertf(4, ddd, nil)))) ==
    {four, 1, 2, 3, {leaf, 1, bbb}, {leaf, 2, ccc}, {leaf, 3, aaa}, {leaf, 4, ddd}},

    insertf(4, aaa, insertf(1, bbb, insertf(2, ccc, insertf(3, ddd, nil)))) ==
    {four, 1, 2, 3, {leaf, 1, bbb}, {leaf, 2, ccc}, {leaf, 3, ddd}, {leaf, 4, aaa}},

    %% FOUR NODE SPLIT TEST %%
    insertf(14, grk, {two, 7,
      {three,2,5,
        {leaf,2,foo},
        {leaf,5,bar},
        {leaf,7,zot}},
      {three,13,16,
        {leaf,13,foo},
        {leaf,16,bar},
        {leaf,18,zot}}}),

    insert(1, a, insertf(2, b, insertf(3, c, insertf(4, d, nil)))),
    insert(10, a, {three, 3, 6, {two, 1, {leaf, 1, nil}, {leaf, 2, nil}}, {two, 4, {leaf, 4, nil}, {leaf, 5, nil}}, {three, 7, 8, {leaf, 7, nil}, {leaf, 8, nil}, {leaf, 9, nil}}})
  ].
