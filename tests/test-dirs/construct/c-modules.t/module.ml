module type S = sig
  type t = private b and b = A | B of t
  type (-'a, +'b) t' = T of ('a -> 'b)
  type t2 = A | B of string | C of t
  type nonrec r = { lbl1 : t; lbl2 : float list}
  type nonrec n = r and m = float
  type t_ext = ..
  type t_ext += Str of string | A
  type v = [`A of t_ext]

  val i : int
  val f : t -> int

  module Sub : sig
    val y : int
  end

  class type room = object
    val mutable gene : unit

    method scientific : unit -> int
  end
  class croom : room
  module type Another = sig val i : int end

  module type Sig = sig
    type t and b
    val f : int -> float
    module type STyp = sig end
    module D : Another
  end

  module Submod : Sig

  module SubFunc (M : Sig) : sig val g : unit end
end

module type Small = sig type t = int end

module M : S = _

let m : (module Small) = _

let m = (module _ : Small)
