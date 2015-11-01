open Chapter_5;;
open Chapter_6;;

type rect =
    Rectangle of int * int
  | Square of int

let area (r: rect) =
  match r with
    Rectangle (w, h) -> w * h
  | Square w -> w * w

let width (r: rect) =
  match r with
    Rectangle (w, h) -> w
  | Square w -> w

let portrait (r: rect) =
  match r with
    Square _ -> r
  | Rectangle (w, h) ->
    if w <= h then r
    else Rectangle(h, w)

let ordered_rects (rs: rect list) =
  let cmp r1 r2 = (width r1) <= (width r2) in
    sort_cmp (map portrait rs) cmp

type 'a sequence =
    Nil
  | Cons of 'a * 'a sequence

let rec take n s =
  if n <= 0 then Nil else
    match s with
      Nil -> Nil
    | Cons (h, t) -> Cons(h, take (n - 1) t)

let rec drop n s =
  if n <= 0 then s else
  match s with
    Nil -> Nil
  | Cons (h, t) -> drop (n - 1) t

let rec map f s =
  match s with
    Nil -> Nil
  | Cons (h, t) -> Cons ((f h), map f t)
