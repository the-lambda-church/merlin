type server
type context

type client = {
  context : context;
  wd      : string;
  environ : string;
  argv    : string array;
}

(* {1 Server management}
   Listen, accept client and close *)

external server_setup : string -> string -> server option =
  "ml_merlin_server_setup"

external server_accept : server -> timeout:float -> client option =
  "ml_merlin_server_accept"

external server_close : server -> unit =
  "ml_merlin_server_close"

(* {1 Context management (stdin, stdout, stderr)}
   Setup and close *)

external context_setup : context -> unit =
  "ml_merlin_context_setup"

external context_close : context -> return_code:int -> unit =
  "ml_merlin_context_close"

(* {1 Environment management} *)

external merlin_set_environ : string -> unit =
  "ml_merlin_set_environ"
(** completely replace the environment *)
