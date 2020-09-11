(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

let errors : (exn list ref * unit Btype.TypeHash.t) option ref = ref None

let monitor_errors' = ref (ref false)
let monitor_errors () =
  if !(!monitor_errors') then
    monitor_errors' := (ref false);
  !monitor_errors'

let raise_error ?(ignore_unify=false) exn =
  !monitor_errors' := true;
  match !errors with
  | Some (l,_) ->
    begin match exn with
      | Ctype.Unify _ when ignore_unify -> ()
      | Ctype.Unify _ | Failure _ ->
        Logger.log ~section:"Typing_aux.raise_error"
          ~title:(Printexc.exn_slot_name exn) "%a"
          Logger.fmt (fun fmt ->
              Printexc.record_backtrace true;
              Format.pp_print_string  fmt (Printexc.get_backtrace ())
            )
      | exn -> l := exn :: !l
    end
  | None -> raise exn

let () =
  Msupport_parsing.msupport_raise_error := raise_error

exception Resume

let resume_raise exn =
  raise_error exn;
  raise Resume

let catch_errors warnings caught f =
  let warnings' = Warnings.backup () in
  let errors' = !errors in
  Warnings.restore warnings;
  errors := (Some (caught,Btype.TypeHash.create 3));
  Misc.try_finally f
    ~always:(fun () ->
       errors := errors';
       Warnings.restore warnings')

let uncatch_errors f =
  let_ref errors None f

let erroneous_type_register te =
  match !errors with
  | Some (_,h) -> Btype.TypeHash.replace h te ()
  | None -> ()

let erroneous_type_check te =
  match !errors with
  | Some (_,h) -> Btype.TypeHash.mem h te
  | _ -> false

let rec erroneous_expr_check e =
  (erroneous_type_check e.Typedtree.exp_type) ||
  match e.Typedtree.exp_desc with
  | Typedtree.Texp_ident (p,_,_)
    when Ident.name (Path.head p) = "_" -> true
  | Typedtree.Texp_apply (e',_) -> erroneous_expr_check e'
  | _ -> false

exception Warning of Location.t * string

let prerr_warning loc w =
  match !errors with
  | None -> () (*Location.print_warning loc Format.err_formatter w*)
  | Some (l, _) ->
    let ppf, to_string = Format.to_string () in
    Location.print_warning loc ppf w;
    match to_string () with
      | "" -> ()
      | s ->  l := Warning (loc,s) :: !l

let prerr_alert loc w =
  match !errors with
  | None -> () (*Location.print_warning loc Format.err_formatter w*)
  | Some (l, _) ->
    let ppf, to_string = Format.to_string () in
    Location.print_alert loc ppf w;
    match to_string () with
      | "" -> ()
      | s ->  l := Warning (loc,s) :: !l

let () = Location.register_error_of_exn (function
    | Warning (loc, str) -> Some (Location.error ~loc ~source:Location.Warning str)
    | _ -> None
  )

let () = Location.prerr_warning_ref := prerr_warning

let () = Location.prerr_alert_ref := prerr_alert

let flush_saved_types () =
  match Cmt_format.get_saved_types () with
  | [] -> []
  | parts ->
    Cmt_format.set_saved_types [];
    let open Ast_helper in
    let pexp = Exp.constant (Saved_parts.store parts) in
    let pstr = Str.eval pexp in
    [Attr.mk (Saved_parts.attribute) (Parsetree.PStr [pstr])]

let rec get_saved_types_from_attributes = function
  | [] -> []
  | attr :: attrs ->
    let (attr, str) = Ast_helper.Attr.as_tuple attr in
    if attr = Saved_parts.attribute then
      let open Parsetree in
      begin match str with
      | PStr({pstr_desc =
                Pstr_eval ({pexp_desc = Pexp_constant key; _ } ,_)
            ; _ } :: _) ->
          Saved_parts.find key
        | _ -> []
      end
    else
      get_saved_types_from_attributes attrs

let with_warning_attribute ?warning_attribute f =
  match warning_attribute with
  | None -> f ()
  | Some attr -> Builtin_attributes.warning_scope attr f

let with_saved_types ?warning_attribute ?save_part f =
  let saved_types = Cmt_format.get_saved_types () in
  Cmt_format.set_saved_types [];
  try
    let result = with_warning_attribute ?warning_attribute f in
    begin match save_part with
      | None -> ()
      | Some f -> Cmt_format.set_saved_types (f result :: saved_types)
    end;
    result
  with exn ->
    let saved_types'= Cmt_format.get_saved_types () in
    Cmt_format.set_saved_types (saved_types' @ saved_types);
    reraise exn
