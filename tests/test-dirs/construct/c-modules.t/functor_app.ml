module type X_int = sig val x : int end

module Increment (M : X_int) = struct
  let x = M.x + 1
end

module X = Increment(_);;
