open Parsetree

(** Generate an extension node that will be reported as a syntax error by
    Merlin. *)
val syntax_error : string -> Location.t -> extension

(** Physical locations might be too precise for some features.

    For instance in:
       let x = f    in    y
                 ^1    ^2

    Merlin cannot distinguish position ^1 from ^2 in the normal AST,
    because IN doesn't appear in abstract syntax. This is a problem when
    completing, because a different environment should be selected for both
    positions.

    One can add relaxed_location attributes to make some locations closer to
    the concrete syntax.

    Here is the same line annotated with physical and relaxed locations:
       let x = f    in    y
              [ ]        [ ]  -- physical locations for f and y nodes
              [     ][     ]  -- relaxed locations for f and y nodes
*)
val relaxed_location : Location.t -> attribute

(** If some code should be ignored by merlin when reporting information to
    the user, put a hide_node attribute.

    This is useful for generated/desugared code which doesn't correspond to
    anything in concrete syntax (example use-case: encoding of some
    js_of_ocaml constructs).
*)
val hide_node : attribute

(** The converse: when merlin should focus on a specific node of the AST.
    The main use case is also for js_of_ocaml.

    Assuming <code> is translated to:

    let module M = struct
      let prolog = ... (* boilerplate *)

      let code = <mapping-of-code>

      let epilog = ... (* boilerplate *)
    end
    in M.boilerplate

    To make merlin focus on [M.code] and ignore the boilerplate ([M.prolog]
    and [M.epilog]), add a [focus_node] attribute to the [M.code] item.
*)
val focus_node : attribute

(* Projections for merlin attributes and extensions *)

val classify_extension : extension ->
  [`Other | `Syntax_error]

val extract_syntax_error : extension -> string * Location.t

val classify_attribute : attribute ->
  [`Other | `Relaxed_location | `Hide | `Focus]

val extract_relaxed_location : attribute -> Location.t
