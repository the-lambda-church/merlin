module type S = sig
  module type T = sig
    type t
  end

  type lol

  module M : T with type t = lol

  val x : M.t
end
