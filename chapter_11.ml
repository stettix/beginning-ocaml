type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec size tr =
  match tr with
    Lf -> 0
  | Br (_, l, r) -> 1 + size l + size r

let rec sum tr =
  match tr with
    Lf -> 0
  | Br (x, l, r) -> x + sum l + sum r

let max x y =
  if x > y then x else y

let rec maxtree tr =
  match tr with
    Lf -> min_int
  | Br (x, l, r) -> max x (max (maxtree l) (maxtree r))

let rec maxdepth tr =
  match tr with
    Lf -> 0
  | Br (x, l, r) -> 1 + max (maxdepth l) (maxdepth r)

let rec list_of_tree tr =
  match tr with
    Lf -> []
  | Br (x, l, r) -> x :: (list_of_tree l) @ (list_of_tree r)

let rec contains x tr =
  match tr with
    Lf -> false
  | Br (v, l, r) -> v = x or (contains x l) or (contains x r)

let rec flip tr =
  match tr with
    Lf -> Lf
  | Br (v, l, r) -> Br (v, r, l)

let rec eq_shape tr1 tr2 =
  match tr1, tr2 with
    Lf, Lf -> true
  | Br (_, l1, r1), Br (_, l2, r2) -> (eq_shape l1 l2) && (eq_shape r1 r2)
  | _ -> false



