-module(brot).

-compile([export_all]).

test(_, _, _, 1) ->
  0;
test(I, Z0, C, M) ->
  case cmplx:cabs(Z0) >= 2 of
    false ->
      test(I + 1, cmplx:add(cmplx:sqr(Z0), C), C, M - 1);
    true ->
      I
  end.

mandelbrot(C, M) ->
  Z0 = cmplx:new(0,0),
  I = 0,
  test(I, Z0, C, M).

tests() ->
  [
    mandelbrot(cmplx:new(0.8, 0), 30),
    mandelbrot(cmplx:new(0.3, 0), 30),
    mandelbrot(cmplx:new(0.27, 0), 30),
    mandelbrot(cmplx:new(0.26, 0), 30),
    mandelbrot(cmplx:new(0.255, 0), 30),
    mandelbrot(cmplx:new(0.8, 0), 50),
    mandelbrot(cmplx:new(0.3, 0), 50),
    mandelbrot(cmplx:new(0.27, 0), 50),
    mandelbrot(cmplx:new(0.26, 0), 50),
    mandelbrot(cmplx:new(0.255, 0), 50)
  ].
