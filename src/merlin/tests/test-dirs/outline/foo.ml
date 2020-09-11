module Bar = struct
 type t = int
 module type S1 = sig
   type t

   val foo : t -> int
 end
end

class type class_type_a = object
  method a : int -> int
end

class class_b = object
  method b s = s ^ s
end

exception Ex of char

type ('a, 'b) eithery =
  | Lefty of 'a
  | Righty of 'b

type 'a point =
  { x : 'a
  ; y : 'a
  ; z : 'a
  }
