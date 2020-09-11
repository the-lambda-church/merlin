module Blah = struct
  type t =
    | A
    | B
end

let f = function
  | Blah.Q -> ()
