
type _ term =
  | Int : int -> int term
  | Pair : 'a term * 'b term -> ('a * 'b) term
  | Fst : ('a * 'b) term -> 'a term
  | Snd : ('a * 'b) term -> 'b term

let rec eval : type a . a term -> a = function
  | Int n -> n
  | Pair (a, b) -> eval a, eval b
  | Fst p -> fst (eval p)
  | Snd p -> snd (eval p)

