module type S = sig
  type t
end

module Set(Elt : S) : sig
  type elt = Elt.t
  type set = private elt list

  val empty : set
end = struct
  type elt = Elt.t
  type set = elt list

  let empty = []
end

module Str = struct
  include String
  include Set(String)
end

let e = Str.empty
