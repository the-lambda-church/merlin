open Std

(** {1 OCaml commandline parsing} *)

type ocaml = {
  include_dirs         : string list;
  no_std_include       : bool;
  unsafe               : bool;
  classic              : bool;
  principal            : bool;
  real_paths           : bool;
  threads              : [ `None | `Threads | `Vmthreads ];
  recursive_types      : bool;
  strict_sequence      : bool;
  applicative_functors : bool;
  unsafe_string        : bool;
  nopervasives         : bool;
  strict_formats       : bool;
  open_modules         : string list;
  ppx                  : string with_workdir list;
  pp                   : string with_workdir option;
  warnings             : Warnings.state;
}

val dump_ocaml : ocaml -> json


(** {1 Merlin high-level settings} *)

type merlin = {
  build_path  : string list;
  source_path : string list;
  cmi_path    : string list;
  cmt_path    : string list;
  extensions  : string list;
  suffixes    : (string * string) list;
  stdlib      : string option;
  reader      : string list;
  protocol    : [`Json | `Sexp];
  log_file    : string option;
  log_sections: string list;
  config_path : string option;

  exclude_query_dir : bool;

  flags_to_apply : string list with_workdir list;

  flags_applied : string list with_workdir list;

  failures : string list;
  extension_to_reader : (string * string) list
}

val dump_merlin : merlin -> json

(** {1 Some flags affecting queries} *)

type query = {
  filename  : string;
  directory : string;
  printer_width : int;
  verbosity : int;
}

(** {1 Main configuration} *)

type t = {
  ocaml   : ocaml;
  merlin  : merlin;
  query   : query;
}

val initial : t

val dump : t -> json

val get_external_config : string -> t -> t

val normalize : t -> t

val is_normalized : t -> bool

val parse_arguments :
  wd:string ->
  warning:(string -> unit) -> 'a Marg.spec list -> string list ->
  t -> 'a -> t * 'a

val flags_for_completion : unit -> string list

val document_arguments : out_channel -> unit

(** {1 Computing project paths} *)

val source_path : t -> string list

val build_path : t -> string list

val cmt_path : t -> string list

val global_modules : ?include_current:bool -> t -> string list

(** {1 Accessors for other information} *)

val filename : t -> string

val unitname : t -> string
