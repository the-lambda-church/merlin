module M = struct
    type t = AC | BC
end
let _ = M.AC
let _ = let open M in BC
