-module(color).

-compile([export_all]).

convert(Depth, Max) ->
  F = Depth/Max,
  A = F*4,
  X = trunc(A),
  Y = trunc(255*(A-X)),
  {0,255-Y,255}.
