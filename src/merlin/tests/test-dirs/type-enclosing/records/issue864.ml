module X = struct
  type t = { z : int }
end

let f1 _ (x : X.t) = x.z

let f2 (z : string) (x : X.t) = x.X.z

let f3 (z : string) (x : X.t) = x.z
