type _ gadt =
  | I : int gadt
  | S : string gadt

let show : type a. a gadt -> a -> string = function
  | I -> string_of_int
  | S -> None
