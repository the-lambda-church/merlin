(* result didn't exist in 4.02 ... *)

type ('a, 'b) result = Ok of 'a | Error of 'b
