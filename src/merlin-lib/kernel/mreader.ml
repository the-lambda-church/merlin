open Std

type parsetree = [
  | `Interface of Parsetree.signature
  | `Implementation of Parsetree.structure
]

type comment = (string * Location.t)

type result = {
  config        : Mconfig.t;
  lexer_errors  : exn list;
  parser_errors : exn list;
  comments      : comment list;
  parsetree     : parsetree;
  no_labels_for_completion : bool;
}

(* Normal entry point *)

let normal_parse ?for_completion config source =
  let kind =
    let filename = Mconfig.(config.query.filename) in
    let extension =
      match String.rindex filename '.' with
      | exception Not_found -> ""
      | pos -> String.sub ~pos ~len:(String.length filename - pos) filename
    in
    Logger.log ~section:"Mreader" ~title:"run"
      "extension(%S) = %S" filename extension;
    if List.exists ~f:(fun (_impl,intf) -> intf = extension)
        Mconfig.(config.merlin.suffixes)
    then Mreader_parser.MLI
    else Mreader_parser.ML
  in
  Mocaml.setup_config config;
  let lexer =
    let keywords = Extension.keywords Mconfig.(config.merlin.extensions) in
    Mreader_lexer.make Mconfig.(config.ocaml.warnings) keywords config source
  in
  let no_labels_for_completion, lexer = match for_completion with
    | None -> false, lexer
    | Some pos ->
      let pos = Msource.get_lexing_pos source
          ~filename:(Mconfig.filename config) pos
      in
      Mreader_lexer.for_completion lexer pos
  in
  let parser = Mreader_parser.make Mconfig.(config.ocaml.warnings) lexer kind in
  let lexer_errors = Mreader_lexer.errors lexer
  and parser_errors = Mreader_parser.errors parser
  and parsetree = Mreader_parser.result parser
  and comments = Mreader_lexer.comments lexer
  in
  { config; lexer_errors; parser_errors; comments; parsetree;
    no_labels_for_completion; }

(* Pretty-printing *)

type pretty_parsetree = Extend_protocol.Reader.pretty_parsetree
type outcometree = Extend_protocol.Reader.outcometree

let ambient_reader = ref None

let instantiate_reader spec config source = match spec with
  | [] -> ((lazy None), ignore)
  | name :: args ->
    let reader = lazy (Mreader_extend.start name args config source) in
    (reader, (fun () ->
       if Lazy.is_val reader then
         match Lazy.force reader with
         | None -> ()
         | Some reader -> Mreader_extend.stop reader))

let get_reader config =
  let rec find_reader assocsuffixes =
    match assocsuffixes with
    | [] -> []
    | (suffix,reader)::t ->
      if Filename.check_suffix Mconfig.(config.query.filename) suffix then [reader] else find_reader t
  in
  match Mconfig.(config.merlin.reader) with
  (* if a reader flag exists then this is explicitly used disregarding suffix association *)
  | [] -> find_reader Mconfig.(config.merlin.extension_to_reader)
  | x -> x

let mocaml_printer reader ppf otree =
  let str = match reader with
    | lazy (Some reader) -> Mreader_extend.print_outcome otree reader
    | _ -> None
  in
  match str with
  | Some str -> Format.pp_print_string ppf str
  | None -> Mocaml.default_printer ppf otree

let with_ambient_reader config source f =
  let ambient_reader' = !ambient_reader in
  let reader_spec = get_reader config in
  let reader, stop = instantiate_reader reader_spec config source in
  ambient_reader := Some (reader, reader_spec, source);
  Misc.try_finally
    (fun () -> Mocaml.with_printer (mocaml_printer reader) f)
    ~always:(fun () -> ambient_reader := ambient_reader'; stop ())

let try_with_reader config source f =
  let reader_spec = get_reader config in
  let lazy reader, stop =
    match !ambient_reader with
    | Some (reader, reader_spec', source')
      when compare reader_spec reader_spec' = 0 &&
           compare source source' = 0 -> reader, ignore
    | _ -> instantiate_reader reader_spec config source
  in
  match reader with
  | None -> stop (); None
  | Some reader ->
    Misc.try_finally (fun () -> f reader) ~always:stop

let print_pretty config source tree =
  match try_with_reader config source
          (Mreader_extend.print_pretty tree) with
  | Some result -> result
  | None ->
    let ppf, to_string = Std.Format.to_string () in
    let open Extend_protocol.Reader in
    begin match tree with
      | Pretty_case_list       x -> Pprintast.case_list       ppf x
      | Pretty_core_type       x -> Pprintast.core_type       ppf x
      | Pretty_expression      x -> Pprintast.expression      ppf x
      | Pretty_pattern         x -> Pprintast.pattern         ppf x
      | Pretty_signature       x -> Pprintast.signature       ppf x
      | Pretty_structure       x -> Pprintast.structure       ppf x
      | Pretty_toplevel_phrase x -> Pprintast.toplevel_phrase ppf x
    end;
    to_string ()

let default_print_outcome tree =
  Mocaml.default_printer Format.str_formatter tree;
  Format.flush_str_formatter ()

let print_outcome config source tree =
  match try_with_reader config source
          (Mreader_extend.print_outcome tree) with
  | Some result -> result
  | None -> default_print_outcome tree

let print_batch_outcome config source tree =
  match try_with_reader config source
          (Mreader_extend.print_outcomes tree) with
  | Some result -> result
  | None -> List.map ~f:default_print_outcome tree

let reconstruct_identifier config source pos =
  match
    try_with_reader config source
      (Mreader_extend.reconstruct_identifier pos)
  with
  | None | Some [] -> Mreader_lexer.reconstruct_identifier config source pos
  | Some result -> result

(* Entry point *)

let parse ?for_completion config source =
  match
    try_with_reader config source
      (Mreader_extend.parse ?for_completion)
  with
  | Some (`No_labels no_labels_for_completion, parsetree) ->
    let (lexer_errors, parser_errors, comments) = ([], [], []) in
    { config; lexer_errors; parser_errors; comments; parsetree;
      no_labels_for_completion; }
  | None -> normal_parse ?for_completion config source

(* Update config after parse *)

(*
let apply_directives config tree =
  let config = ref config in
  let attribute _ attr =
    let ({Location. txt = name; _}, payload) = Ast_helper.Attr.as_tuple attr in
    match name with
    | "merlin.directive.require" ->
      begin match Ast_helper.extract_str_payload payload with
        | None -> ()
        | Some (package, _) ->
          let open Mconfig in
          let merlin = !config.merlin in
          let merlin = {merlin with packages_to_load =
                                      package :: merlin.packages_to_load} in
          config := {!config with merlin}
      end
    | _ -> ()
  in
  let open Ast_iterator in
  let iterator = {default_iterator with attribute} in
  begin match tree with
    | `Interface sg -> iterator.signature iterator sg
    | `Implementation str -> iterator.structure iterator str
  end;
  !config
*)
let apply_directives config _tree = config
