open Chapter_4;;
open Chapter_5;;

let rec map f l =
  match l with 
    [] -> []
  | h :: t -> f h :: map f t

let clip x =
  if x <= 1 then 1
    else if x >= 10 then 10
    else x

let cliplist l = map clip l

let rec apply f n x =
  if n < 1 then x
    else apply f (n - 1) (f x)

let rec filter pred l =
  match l with
    [] -> []
  | h :: t ->
    if pred h then h :: filter pred t
      else filter pred t

let rec for_all pred l =
  match l with
    [] -> true
  | h :: t -> (pred h) && for_all pred t

let rec mapl f l =
  map (map f) l
