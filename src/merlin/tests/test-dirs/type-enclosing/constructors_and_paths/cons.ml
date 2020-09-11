type t = U
type t' = U

let f : t  = U

let g (x : t) =
  match x with
  | U -> ()

module M = struct
  type t = A
  type u = A | B
end

let f () = (M.A : M.t)

let _ = M.A

module N = struct
  type t = A of int
  let x = 3
end

let _ = Some (N.A 3)

let _ = N.x
