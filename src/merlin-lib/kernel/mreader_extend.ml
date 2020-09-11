open Std
open Extend_protocol.Reader

let {Logger. log} = Logger.for_section "Mreader_extend"

type t = {
  name : string;
  args : string list;
  config : Mconfig.t;
  source : Msource.t;
  driver : Extend_driver.t;
  mutable stopped : bool;
}

let print () t = t.name

let incorrect_behavior fn t =
  log ~title:fn "Extension %S has incorrect behavior" t.name

let stop t =
  if t.stopped then
    log ~title:"stop" "%a: already closed" print t
  else (
    log ~title:"stop" "%a" print t;
    t.stopped <- true;
    Extend_driver.stop t.driver
  )

let stop_finalise t =
  if not t.stopped then (
    log ~title:"stop_finalise" "leaked process %s" t.name;
    stop t
  )

let load_source t config source =
  let buffer = {
    path  = Mconfig.filename config;
    flags = t.args;
    text  = Msource.text source;
  } in
  match Extend_driver.reader t.driver (Req_load buffer) with
  | Res_loaded -> Some t
  | _ ->
    Extend_driver.stop t.driver;
    incorrect_behavior "load_source" t;
    None

let start name args config source =
  let section = "(ext)" ^ name in
  let notify str = Logger.notify ~section "%s" str in
  let debug str = Logger.log ~section:"reader" ~title:section "%s" str in
  let driver = Extend_driver.run ~notify ~debug name in
  let process = { name; args; config; source; driver; stopped = false } in
  Gc.finalise stop_finalise process;
  load_source process config source

let parsetree = function
  | Signature sg -> `Interface sg
  | Structure str -> `Implementation str

let parse ?for_completion t =
  log ~title:"parse" "?for_completion:%a %a"
    (Option.print Msource.print_position) for_completion
    print t;
  assert (not t.stopped);
  match
    Extend_driver.reader t.driver
      (match for_completion with
       | None -> Req_parse
       | Some pos ->
         let pos = Msource.get_lexing_pos t.source
             ~filename:(Mconfig.filename t.config) pos
         in
         Req_parse_for_completion pos)
  with
  | Res_parse ast ->
    Some (`No_labels false, parsetree ast)
  | Res_parse_for_completion (info, ast) ->
    Some (`No_labels (not info.complete_labels), parsetree ast)
  | _ ->
    incorrect_behavior "parse" t;
    None

let reconstruct_identifier pos t =
  log ~title:"reconstruct_identifier" "%a %a"
    Lexing.print_position pos print t;
  match Extend_driver.reader t.driver (Req_get_ident_at pos) with
  | Res_get_ident_at ident -> Some ident
  | _ ->
    incorrect_behavior "reconstruct_identifier" t;
    None

let attr_cleaner =
  let open Ast_mapper in
  let attributes mapper attrs =
    let not_merlin_attribute attr =
      let (name,_) = Ast_helper.Attr.as_tuple attr in
      not (String.is_prefixed ~by:"merlin." name.Location.txt) in
    let attrs = List.filter ~f:not_merlin_attribute attrs in
    default_mapper.attributes mapper attrs
  in
  { default_mapper with attributes }

let clean_tree =
  let open Ast_mapper in function
  | Pretty_case_list x ->
    Pretty_case_list (attr_cleaner.cases attr_cleaner x)
  | Pretty_core_type x ->
    Pretty_core_type (attr_cleaner.typ attr_cleaner x)
  | Pretty_expression x ->
    Pretty_expression (attr_cleaner.expr attr_cleaner x)
  | Pretty_pattern x ->
    Pretty_pattern (attr_cleaner.pat attr_cleaner x)
  | Pretty_signature x ->
    Pretty_signature (attr_cleaner.signature attr_cleaner x)
  | Pretty_structure x ->
    Pretty_structure (attr_cleaner.structure attr_cleaner x)
  | Pretty_toplevel_phrase (Parsetree.Ptop_def x) ->
    let x = attr_cleaner.structure attr_cleaner x in
    Pretty_toplevel_phrase (Parsetree.Ptop_def x)
  | Pretty_toplevel_phrase (Parsetree.Ptop_dir _) as tree -> tree

let print_pretty tree t =
  log ~title:"print_pretty" "TODO %a" print t;
  let tree = clean_tree tree in
  match Extend_driver.reader t.driver (Req_pretty_print tree) with
  | Res_pretty_print str -> Some str
  | _ ->
    incorrect_behavior "pretty_print" t;
    None

let print_outcomes ts t =
  log ~title:"print_outcomes" "TODO %a" print t;
  match ts with
  | [] -> Some []
  | ts -> match Extend_driver.reader t.driver (Req_print_outcome ts) with
    | Res_print_outcome ts -> Some ts
    | _ ->
      incorrect_behavior "print_batch_outcome" t;
      None

let print_outcome o t =
  log ~title:"print_outcome" "TODO %a" print t;
  match Extend_driver.reader t.driver (Req_print_outcome [o]) with
  | Res_print_outcome [o] -> Some o
  | _ ->
    incorrect_behavior "print_batch_outcome" t;
    None
