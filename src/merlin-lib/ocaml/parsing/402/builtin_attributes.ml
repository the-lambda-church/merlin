open Asttypes
open Parsetree

let string_of_cst = function
  | Pconst_string(s, _) -> Some s
  | _ -> None

let string_of_payload = function
  | PStr[{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant c},_)}] ->
      string_of_cst c
  | _ -> None

let warning_scope = ref []

let warning_enter_scope () =
  warning_scope := (Warnings.backup ()) :: !warning_scope
let warning_leave_scope () =
  match !warning_scope with
  | [] -> assert false
  | hd :: tl ->
      Warnings.restore hd;
      warning_scope := tl

let warning_attribute attrs =
  let process loc txt errflag payload =
    match string_of_payload payload with
    | Some s ->
        begin try Warnings.parse_options errflag s
        with Arg.Bad _ ->
          Location.prerr_warning loc
            (Warnings.Attribute_payload
               (txt, "Ill-formed list of warnings"))
        end
    | None ->
        Location.prerr_warning loc
          (Warnings.Attribute_payload
             (txt, "A single string literal is expected"))
  in
  List.iter
    (function
      | ({txt = ("ocaml.warning"|"warning") as txt; loc}, payload) ->
          process loc txt false payload
      | ({txt = ("ocaml.warnerror"|"warnerror") as txt; loc}, payload) ->
          process loc txt true payload
      | _ ->
          ()
    )
    attrs

let with_warning_attribute attrs f =
  warning_enter_scope ();
  warning_attribute attrs;
  match f () with
  | result ->
    warning_leave_scope ();
    result
  | exception exn ->
    warning_leave_scope ();
    Std.reraise exn

let warning_scope ?ppwarning:_ attrs f =
  let prev = Warnings.backup () in
  try
    warning_attribute attrs;
    let ret = f () in
    Warnings.restore prev;
    ret
  with exn ->
    Warnings.restore prev;
    raise exn
