type t = { foo : int }

let f t = t.foo

let foo () = 3

let f t = t.foo

module X = struct
  type t = { bar : int; baz : bool }
end

let bar = 123
let baz = true
let y = { X.bar ; baz }
