type t = int64

let max_idx = 63
let empty = 0L
let full = -1L
let bit idx =
  if idx >= 64 then assert false;
  Int64.shift_left 1L idx
let set idx t = Int64.logor t (bit idx)
let unset idx t = Int64.logand t (Int64.lognot (bit idx))
let is_set t idx = Int64.logand t (bit idx) <> 0L
