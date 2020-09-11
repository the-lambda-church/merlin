(** {0 Argument parsing library which fold over arguments}

    Specifications of arguments is split in two passes:
    - [_ table] for parsing global arguments (compiler flags, merlin
      configuration)
    - a (string * _ t) for parsing command local arguments
*)

(** Action associated to a flag updating a state of type 'acc.
    It takes a list of arguments and either succeeds returning untouched
    arguments or fails raising an exception. *)
type 'acc t = string list -> 'acc -> string list * 'acc

(** A table mapping a flag to the corresponding action *)
type 'acc table = (string, 'acc t) Hashtbl.t

(** {1 Combinators for building actions} *)

(** Action updating state and not consuming any argument *)
val unit : ('acc -> 'acc) -> 'acc t

(** Action consuming a single argument *)
val param : string -> (string -> 'acc -> 'acc) -> 'acc t

(** Action consuming a boolean argument *)
val bool : (bool -> 'acc -> 'acc) -> 'acc t

(** Action doing nothing *)
val unit_ignore : 'acc t

(** Action doing nothing and dropping one argument *)
val param_ignore : 'acc t

(** {1 Parsing of argument lists} *)

type docstring = string

type 'a spec = (string * docstring * 'a t)

(** Consume at most one flag from the list, returning updated state or
    [None] in case of failure.
    Warning function is called with an error message in case of incorrect
    use.  *)
val parse_one :
  warning:(string -> unit) ->
  'global table -> 'local spec list ->
  string list -> 'global -> 'local ->
  (string list * 'global * 'local) option

(** Consume all arguments from the input list, calling warning for incorrect
    ones and resuming parsing after. *)
val parse_all :
  warning:(string -> unit) ->
  'global table -> 'local spec list ->
  string list -> 'global -> 'local ->
  'global * 'local
