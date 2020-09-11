type t = { mutable b: float }

let b = 10
let x = { b = 9. }

let y = { c = 9. }

let _ = x.b <- 3.

type foo = Bar of {baz : unit}

let z = Bar ({ baz = () })
