type t = { foo : int }

let foo = 5
let x = { foo }

(* introducing a syntax error, just to see. *)

let foo = 5 in
let x = { foo }
