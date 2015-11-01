type rect =
    Rectangle of int * int
  | Square of int

let area (r: rect) =
  match r with
    Rectangle (w, h) -> w * h
  | Square w -> w * w

let portrait (r: rect) =
  match r with
    Square _ -> r
  | Rectangle (w, h) ->
    if w <= h then r
    else Rectangle(h, w)
