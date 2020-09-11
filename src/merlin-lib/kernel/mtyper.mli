(** {1 Result of typechecker}

    [Mtyper] essentially produces a typedtree, but to make sense of it
    the OCaml typechecker need to be in a specific state.

    The [result] type wraps a snapshot of this state with the typedtree to
    ensure correct accesses.
*)

type result

type typedtree = [
  | `Interface of Typedtree.signature
  | `Implementation of Typedtree.structure
]

val run : Mconfig.t -> Mreader.parsetree -> result

val get_env : ?pos:Msource.position -> result -> Env.t

val get_typedtree : result -> typedtree

val get_errors : result -> exn list

(** Heuristic to find suitable environment to complete / type at given position.
 *  1. Try to find environment near given cursor.
 *  2. Check if there is an invalid construct between found env and cursor :
 *    Case a.
 *      > let x = valid_expr ||
 *      The env found is the right most env from valid_expr, it's a correct
 *      answer.
 *    Case b.
 *      > let x = valid_expr
 *      > let y = invalid_construction||
 *      In this case, the env found is the same as in case a, however it is
 *      preferable to use env from enclosing module rather than an env from
 *      inside x definition.
 *)
val node_at :
  ?skip_recovered:bool -> result -> Lexing.position -> Mbrowse.t
