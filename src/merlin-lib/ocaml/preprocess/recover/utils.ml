let const c = fun _ -> c

let group_assoc l =
  let cons k v acc = (k, List.rev v) :: acc in
  let rec aux k v vs acc = function
    | [] -> List.rev (cons k (v :: vs) acc)
    | (k', v') :: xs when compare k k' = 0 ->
      if compare v v' = 0 then
        aux k v vs acc xs
      else
        aux k v' (v :: vs) acc xs
    | (k', v') :: xs ->
      aux k' v' [] (cons k (v :: vs) acc) xs
  in
  match List.sort compare l with
  | [] -> []
  | (k, v) :: xs -> aux k v [] [] xs

(* negation to put nan as the max *)
let compare_float a b = - compare (-.a) (-.b)

let min_float a b =
  if compare_float a b > 0 then b else a

let arg_min_float f a b =
  if compare_float (f a) (f b) <= 0 then a else b

exception Found of int
let array_exists arr f =
  try
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done;
    false
  with Found _ -> true

let array_findi arr f =
  match
    for i = 0 to Array.length arr - 1 do
      if f arr.(i) then raise (Found i);
    done
  with () -> raise Not_found
     | exception (Found i) -> i

let array_find arr f =
  arr.(array_findi arr f)

let array_assoc arr x =
  snd (array_find arr (fun (x',_) -> compare x x' = 0))

let list_fmt f l =
  "[" ^ String.concat "; " (List.map f l) ^ "]"

let fst3 (x,_,_) = x

let rec list_last = function
  | [x] -> x
  | _ :: xs -> list_last xs
  | [] -> invalid_arg "list_last"
