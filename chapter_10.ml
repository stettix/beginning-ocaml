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


type expr =
  Num of int
| Add of expr * expr
| Subtract of expr * expr
| Multiply of expr * expr
| Divide of expr * expr
| Pow of expr * expr

let rec evaluate e =
  match e with
    Num n -> n
  | Add (e, e') -> evaluate e + evaluate e'
  | Subtract (e, e') -> evaluate e - evaluate e'
  | Multiply (e, e') -> evaluate e * evaluate e'
  | Divide (e, e') -> evaluate e / evaluate e'
  | Pow (e, e') -> int_of_float((float_of_int (evaluate e)) ** (float_of_int (evaluate e')))
