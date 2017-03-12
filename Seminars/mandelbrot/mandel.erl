-module(mandel).

-compile([export_all]).

rows(_, 0, _, _, L) -> L;
rows(Width, Height, Trans, Depth, L) ->
  Row = lists:map(
    fun(E) ->
      ComplexNum = Trans(E, Height),
      DepthCmplxNum = brot:mandelbrot(ComplexNum, Depth),
      color:convert(DepthCmplxNum, Depth)
    end,
    lists:seq(0, Width)),
  rows(Width, Height - 1, Trans, Depth, [Row | L]).

mandelbrot(Width, Height, X, Y, K, Depth) ->
  Trans =
  fun (W, H) ->
    cmplx:new(X+K*(W-1), Y-K*(H-1))
  end,
  rows(Width, Height, Trans, Depth, []).

demo() ->
  small(-2.6, 1.2, 1.6).

small(X, Y, X1) ->
  %Width = 960,
  %Height = 540,
  Width = 1920,
  Height = 1080,
  %Width = 10,
  %Height = 10,
  K = (X1 - X)/Width,
  Depth = 64,
  T0 = now(),
  Image = mandelbrot(Width, Height, X, Y, K, Depth),
  T = timer:now_diff(now(), T0),
  io:format("picture generated in ~w ms~n", [T div 1000]),
  ppm:write("small.ppm", Image).
