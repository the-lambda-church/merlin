(** {0 OCaml compiler compatible command-line parameters}

    For compatibility with typechecker.
    Argument parsing / build environment construction happens elsewhere.
*)

(** {1 Relevant settings}
    Parameters from OCaml compiler which affect Merlin behavior. *)
val include_dirs         : string list ref
val fast                 : bool ref
val classic              : bool ref
val principal            : bool ref
val real_paths           : bool ref
val recursive_types      : bool ref
val strict_sequence      : bool ref
val applicative_functors : bool ref
val unsafe_string        : bool ref
val nopervasives         : bool ref
val strict_formats       : bool ref
val open_modules         : string list ref

(** {1 Dummy values}
    Ignored by merlin but kept for compatibility with upstream code. *)
val annotations          : bool ref
val binary_annotations   : bool ref
val print_types          : bool ref
val native_code          : bool ref
val dont_write_files     : bool ref
val error_size           : int ref (* max size of module related errors *)
val keep_locs            : bool ref
val keep_docs            : bool ref
val transparent_modules  : bool ref
val for_package          : string option ref
val debug                : bool ref
val opaque               : bool ref
val unboxed_types        : bool ref

val locations            : bool ref
