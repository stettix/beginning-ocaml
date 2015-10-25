let rec numkeys d =
  match d with
    [] -> 0
  | h :: t -> 1 + (numkeys t)

let rec replace k v d =
  match d with
    (hk, hv) :: t ->
      if hk = k then (k, v) :: t
        else (hk, hv) :: replace k v t
  | _ -> raise Not_found

let rec dict_of_lists l1 l2 =
  match l1, l2 with
    [], [] -> []
  | h1 :: t1, h2 :: t2 -> (h1, h2) :: dict_of_lists t1 t2
  | _ -> raise (Invalid_argument "Lists not of equal size")
