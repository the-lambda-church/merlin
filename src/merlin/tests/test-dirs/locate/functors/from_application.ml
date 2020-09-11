module Make(X : sig type t end) = struct
  type t = X.t list
end

module A = struct
  type t = int
end

type a = A

module M1 = Make(A)

module M2 = Make(struct
    type indir = a

    let _noise = ()

    type t = indir
  end)
