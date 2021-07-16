let z  = Foo.x
let _ = Foo.M.y

let _ = z

module J = Foo

module F = Foo.F (Foo.M) (struct let z = "foo" end)

let _ = F.y
let _ = F.z