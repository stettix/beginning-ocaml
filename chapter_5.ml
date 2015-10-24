open Chapter_4;;

let rec merge x y =
  match x, y with
    [], l -> l
  | l, [] -> l
  | hx :: tx, hy :: ty ->
    if hx < hy
  	  then hx :: merge tx (hy :: ty)
  	  else hy :: merge ty (hx :: tx)

let rec msort l =
  match l with
    [] -> []
  | [x] -> [x]
  | _ ->
      let half = length l / 2 in
        let left = take half l in
          let right = drop half l in
            merge (msort left) (msort right)

let rec insert x l cmp =
  match l with
    [] -> [x]
  | h :: t ->
    if cmp x h then x :: l
      else h :: (insert x t cmp)

let rec sort l =
  match l with
    [] -> []
  | h :: t -> insert h (sort t) (<=)

let rec rev_sort l =
  match l with
    [] -> []
  | h :: t -> insert h (rev_sort t) (>=)

let rec is_sorted l =
  match l with
    x :: y :: t -> (x <= y) && (is_sorted (y :: t))
  | _ -> true
