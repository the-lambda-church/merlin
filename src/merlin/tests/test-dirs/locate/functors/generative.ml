module type S = sig

  val foo : int -> int

end

module Make (Foo : sig type t end) (Bar : sig end) () : S = struct
  let foo x = x + 1
end

module M = Make(struct type t = int end)(struct end)()

let x = M.foo
