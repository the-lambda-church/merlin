type a = A

module type a = sig end

let a = A

module type A = a

module A : A = struct end

exception A

type r = { a : a }

let test =
  match ({ a }.a : a) with
  | A -> ()
  | exception A -> ()

exception B = A

let test = (* don't stress the parser so much *)
  let a = { a = A } in
  a.a
