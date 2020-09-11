module type S = sig
  type t
end

module Identity(X : S) : S = X

module Apply(Id : S -> S) = Id

module Simple = struct
  type t
end

module M1 = Identity(Identity(Simple))

module M2 = Apply(Identity)(Simple)

type t1 = M1.t

type t2 = M2.t

module Alternative_apply(Id : S -> S)(X : S) = struct include Id(X) end

module M3 = Alternative_apply(Identity)(Simple)

type t3 = M3.t

module M4 = Alternative_apply(functor(X : S) -> X)(Simple)

type t4 = M4.t

module M5 = Identity((functor(X : S) -> X)(Simple))

type t5 = M5.t
