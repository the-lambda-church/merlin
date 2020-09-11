(* First a typing error *)

let () = 3

(* Then a parsing error *)

let () = | 3

(* Then a typing error again *)

let () = 3
