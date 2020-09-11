(* Dynamic-scoping for global piece of state *)

type bindings
val new_bindings : unit -> bindings
val is_bound : bindings -> bool
val reset : bindings -> unit

val ref : bindings -> (unit -> 'a) -> 'a ref

type scope
val fresh : bindings -> scope
val with_scope : scope -> (unit -> 'a) -> 'a

(* ... Unique instance for compiler-libs state *)

module Compiler : sig
  val compiler_state : bindings
  val sref : (unit -> 'a) -> 'a ref
  val srefk : 'a -> 'a ref
end
