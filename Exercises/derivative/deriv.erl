-module(deriv).

-compile(export_all).

deriv({const, _}, _) -> {const, 0};

deriv({var, V}, V) -> {const, 1};

deriv({var, _}, _) -> {const, 0};

deriv({add, E1, E2}, V) ->
  {add, deriv(E1, V), deriv(E2, V)};

deriv({mul, E1, E2}, V) ->
  {add, {mul, deriv(E1, V), E2}, {mul, E1, deriv(E2, V)}};

deriv({pow, {var, B}, {Expr, Val}}, B) ->
  {mul, {const, Val}, {pow, B, {Expr, Val - 1}}};

deriv({ln, x}, x) ->
  {mdiv, {const, 1}, {var, x}};

deriv({mdiv, 1, x}, x) ->
  {mdiv, {const, -1}, {pow, {var, x}, {const, 2}}};

deriv({sqrt, x}, x) ->
  {mdiv, {const, 1}, {mul, {const, 2}, {sqrt, {var, x}}}};

deriv({sin, x}, x) ->
  {cos, x}.

%% ---------- DEBUGGING TESTS ---------- %%
addTest() -> deriv({add, {var, x}, {var, x}}, x).
mulTest() -> deriv({mul, {var, x}, {var, x}}, x).
test() -> deriv({add, {mul, {const, 2}, {var, x}}, {const, 3}}, x).
