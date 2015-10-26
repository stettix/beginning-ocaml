open Chapter_4;;

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

let rec split d =
  match d with
    [] -> [], []
  | (k, v) :: t ->
    let tk, tv = split t in
      k :: tk, v :: tv 

let rec dict_of_pairs l = 
  let rec loop l seenKeys =
    match l, seenKeys with
	  [], _ -> []
	| ((k, v) :: t), _ ->
	  if member k seenKeys then loop t seenKeys
	    else (k, v) :: (loop t (k :: seenKeys))
  in loop l []
