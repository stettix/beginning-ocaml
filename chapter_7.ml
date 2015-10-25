open Chapter_4;;
open Chapter_5;;
open Chapter_6;;

let rec smallest l =
  match l with
    [] -> raise Not_found
  | [x] -> x
  | h :: t ->
    let r = smallest(t) in
      if h < r then h else r

let smallest_or_zero l =
  try smallest l with Not_found -> 0

exception Negative_argument

let intsqrt n =
  if n >= 0 then int_of_float(sqrt(float_of_int n))
    else raise Negative_argument
