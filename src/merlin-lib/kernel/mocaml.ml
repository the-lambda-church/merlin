open Std
open Local_store.Compiler

(* Instance of environment cache & btype unification log  *)

type typer_state = Local_store.scope

let current_state = srefk None

let new_state () =
  let scope = Local_store.fresh compiler_state in
  Local_store.with_scope scope (fun () -> current_state := Some scope);
  scope

let with_state state f =
  if Local_store.is_bound compiler_state then
    failwith "Mocaml.with_state: another instance is already in use";
  match Local_store.with_scope state f with
  | r -> Cmt_format.clear (); r
  | exception exn -> Cmt_format.clear (); reraise exn

let is_current_state state = match !current_state with
  | Some state' -> state == state'
  | None -> false

(* Build settings *)

let setup_config config = (
  assert Local_store.(is_bound compiler_state);
  let open Mconfig in
  let open Clflags in
  let ocaml = config.ocaml in
  Load_path.init (Mconfig.build_path config);
  Env.set_unit_name (Mconfig.unitname config);
  Location.input_name  := config.query.filename;
  fast                 := ocaml.unsafe ;
  classic              := ocaml.classic ;
  principal            := ocaml.principal ;
  real_paths           := ocaml.real_paths ;
  recursive_types      := ocaml.recursive_types ;
  strict_sequence      := ocaml.strict_sequence ;
  applicative_functors := ocaml.applicative_functors ;
  unsafe_string        := ocaml.unsafe_string ;
  nopervasives         := ocaml.nopervasives ;
  strict_formats       := ocaml.strict_formats ;
  open_modules         := ocaml.open_modules ;
)

(** Switchable implementation of Oprint *)

let default_out_value          = !Oprint.out_value
let default_out_type           = !Oprint.out_type
let default_out_class_type     = !Oprint.out_class_type
let default_out_module_type    = !Oprint.out_module_type
let default_out_sig_item       = !Oprint.out_sig_item
let default_out_signature      = !Oprint.out_signature
let default_out_type_extension = !Oprint.out_type_extension
let default_out_phrase         = !Oprint.out_phrase

let replacement_printer = ref None

let oprint default inj ppf x = match !replacement_printer with
  | None -> default ppf x
  | Some printer -> printer ppf (inj x)

let () =
  let open Extend_protocol.Reader in
  Oprint.out_value :=
    oprint default_out_value (fun x -> Out_value x);
  Oprint.out_type :=
    oprint default_out_type (fun x -> Out_type x);
  Oprint.out_class_type :=
    oprint default_out_class_type (fun x -> Out_class_type x);
  Oprint.out_module_type :=
    oprint default_out_module_type (fun x -> Out_module_type x);
  Oprint.out_sig_item :=
    oprint default_out_sig_item (fun x -> Out_sig_item x);
  Oprint.out_signature :=
    oprint default_out_signature (fun x -> Out_signature x);
  Oprint.out_type_extension :=
    oprint default_out_type_extension (fun x -> Out_type_extension x);
  Oprint.out_phrase :=
    oprint default_out_phrase (fun x -> Out_phrase x)

let default_printer ppf =
  let open Extend_protocol.Reader in function
  | Out_value x          -> default_out_value ppf x
  | Out_type x           -> default_out_type ppf x
  | Out_class_type x     -> default_out_class_type ppf x
  | Out_module_type x    -> default_out_module_type ppf x
  | Out_sig_item x       -> default_out_sig_item ppf x
  | Out_signature x      -> default_out_signature ppf x
  | Out_type_extension x -> default_out_type_extension ppf x
  | Out_phrase x         -> default_out_phrase ppf x


let with_printer printer f =
  let_ref replacement_printer (Some printer) f

(* Cleanup caches *)
let clear_caches () = (
  Cmi_cache.clear ();
  Cmt_cache.clear ();
  Directory_content_cache.clear ();
)

(* Flush cache *)
let flush_caches ?older_than () = (
  Cmi_cache.flush ?older_than ();
  Cmt_cache.flush ?older_than ()
)
