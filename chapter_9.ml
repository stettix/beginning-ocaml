(*

The function g a b c has type A -> B -> C -> D which can be written A -> (B -> (C -> D)).
Thus, it takes an argument of type A and returns a function of type B -> (C -> D) which, when you give it
an argument of type B returns a function which when you give it an argument of type C returns something
of type D. And so, we can apply just one or two arguments to the function f (which is called partial application),
or apply all thre arguments at once. When we write let g a b c = ... this is just shorthand for
let g = fun a -> fun b -> fun c -> ...

*)

open Chapter_4;;
open Chapter_6;;

(* member: a' -> a' list -> boolean *)
(* member_all: a' -> a' list list -> boolean *)
let member_all x ls =
  let ms = map (member x) ls in
    not (member false ms)

let mapll f =
  map (map (map f))

let truncate n ls =
  map (take n) ls

let heads default ls =
  let head_or_default d l =
    match l with
    [] -> d
   | h :: _ -> h in
  map (head_or_default default) ls
