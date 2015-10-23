let rec evens l = 
  match l with 
    [] -> []
  | [h] -> [h]
  | h :: _ :: t -> h :: evens t

let rec count_true l = 
  match l with
    [] -> 0
  | true :: tail -> 1 + count_true tail
  | false :: tail -> count_true tail

let rec rev l =
  match l with
    [] -> []
  | h :: t -> rev t  @ [h]

let rec palindrome l =
  match l with
    [] -> []
  | [h] -> [h]
  | h :: t -> h :: palindrome t @ [h]

let rec is_palindrome l =
  rev l = l

let rec drop_last l =
  match l with
    [] -> []
  | [h] -> []
  | h :: t -> h :: drop_last t

let rec drop_last_inner l acc = ???
  match l with
    [] -> acc
  | [h] -> acc


let rec drop_last_tr l =
  drop_last_inner l []
