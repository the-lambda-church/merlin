module M = struct
  (* should jump above this comment,
     but jumps below it pre 4.08 *)
  module M = struct end
end
open M
