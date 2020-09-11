module A = struct let (+.) a b = a +. b end
let f x = A.(x +. 1.)
