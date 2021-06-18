exception Not_allowed of string
exception Not_a_hole

type values_scope = Null | Local

val node
  : ?depth : int
  -> keywords : string list
  -> values_scope : values_scope
  -> Browse_raw.node
  -> string list
