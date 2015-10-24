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
      let left = take (length l / 2) l in
        let right = drop (length l / 2) l in
          merge (msort left) (msort right)
