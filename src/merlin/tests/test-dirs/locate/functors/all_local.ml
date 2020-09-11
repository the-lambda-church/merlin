module type S = sig
  type t
end

module M = struct
  type t = T
end

module Make(Arg : S) : S = struct
  include Arg

  type x = t

  let foo : x -> x =
    fun x -> x
end

module Foo = Make(M)

type t = Foo.t
