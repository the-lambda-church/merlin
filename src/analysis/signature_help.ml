open Std

open Browse_raw

open Extend_protocol.Reader

type parameter_info =
  { label : Asttypes.arg_label
  ; param_start : int
  ; param_end : int
  ; argument : Typedtree.expression option
  }

type application_signature =
  { function_name : string option
  ; function_position : Msource.position
  ; signature : string
  ; parameters : parameter_info list
  ; active_param : int option
  }

(* extract a properly properly parenthesized identifier
   from (expression_desc (Texp_ident (Longident))) *)
let extract_ident (exp_desc : Typedtree.expression_desc) =
  let rec longident ppf : Longident.t -> unit = function
    | Lident s -> fprintf ppf "%s" (Completion.parenthesize_name s)
    | Ldot (p, s) -> fprintf ppf "%a.%s" longident p (Completion.parenthesize_name s)
    | Lapply (p1, p2) -> fprintf ppf "%a(%a)" longident p1 longident p2
  in
  match exp_desc with
  | Texp_ident (_, { txt = li; _ }, _) ->
    let ppf, to_string = Format.to_string () in
    longident ppf li;
    Some (to_string ())
  | _ -> None

(* Type variables shared across arguments should all be
   printed with the same name.
   [Printtyp.type_scheme] ensure that a name is unique within a given
   type, but not across different invocations.
   [reset] followed by calls to [mark_loops] and [type_sch] provide
   that *)
let pp_type env ppf ty =
  let module Printtyp = Type_utils.Printtyp in
  Printtyp.wrap_printing_env env ~verbosity:0 (fun () ->
    Printtyp.mark_loops ty;
    Printtyp.type_sch ppf ty)

(* surround function types in parentheses *)
let pp_parameter_type env ppf ty =
  match ty with
  | { Types.desc = Tarrow _; _ } as ty ->
    Format.fprintf ppf "(%a)" (pp_type env) ty
  | ty -> pp_type env ppf ty

(* print parameter labels and types *)
let pp_parameter env label ppf ty =
  match label with
  | Asttypes.Nolabel ->
    pp_parameter_type env ppf ty
  | Asttypes.Labelled l ->
    Format.fprintf ppf "%s:%a" l (pp_parameter_type env) ty
  | Asttypes.Optional l ->
    (* unwrap option for optional labels the same way as
       [Raw_compat.labels_of_application] *)
    let unwrap_option ty = match (Ctype.repr ty).Types.desc with
      | Types.Tconstr (path, [ty], _)
        when Path.same path Predef.path_option -> ty
      | _ -> ty
    in
    Format.fprintf ppf "?%s:%a" l (pp_parameter_type env) (unwrap_option ty)

(* record buffer offsets to be able to underline parameter types *)
let print_parameter_offset ?arg:argument ppf buffer env label ty =
  let param_start = Buffer.length buffer in
  Format.fprintf ppf "%a%!" (pp_parameter env label) ty;
  let param_end = Buffer.length buffer in
  Format.pp_print_string ppf " -> ";
  Format.pp_print_flush ppf ();
  { label; param_start; param_end; argument }

let separate_function_signature ~args (e : Typedtree.expression) =
  Type_utils.Printtyp.reset ();
  let buffer = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer buffer in
  let rec separate ?(i=0) ?(parameters=[]) args ty =
    match (args, ty) with
    | (l, arg)::args, { Types.desc = Tarrow (label, ty1, ty2, _) } ->
      let parameter = print_parameter_offset ppf buffer e.exp_env label ty1 ?arg in
      separate args ty2 ~i:(succ i) ~parameters:(parameter::parameters)

    | [], { Types.desc = Tarrow (label, ty1, ty2, _) } ->
      let parameter = print_parameter_offset ppf buffer e.exp_env label ty1 in
      separate args ty2 ~i:(succ i) ~parameters:(parameter::parameters)

    (* end of function type, print remaining type without recording offsets *)
    | _ ->
      Format.fprintf ppf
        "%a%!" (pp_type e.exp_env) ty;
      { function_name = extract_ident e.exp_desc
      ; function_position = `Offset e.exp_loc.loc_end.pos_cnum
      ; signature = Buffer.contents buffer
      ; parameters = List.rev parameters
      ; active_param = None
      }
  in
  separate args e.exp_type

let active_parameter_by_arg ~arg params =
  let find_by_arg = function
    | { argument = Some a; _ } when a == arg -> true
    | _ -> false
  in
  try Some (List.index params ~f:find_by_arg)
  with Not_found -> None

let active_parameter_by_prefix ~prefix params =
  let common = function
    | Asttypes.Nolabel -> Some 0
    | l when String.is_prefixed ~by:"~" prefix
          || String.is_prefixed ~by:"?" prefix ->
      Some (String.common_prefix_len (Btype.prefixed_label_name l) prefix)
    | _ -> None
  in

  let rec find_by_prefix ?(i=0) ?longest_len ?longest_i = function
    | [] -> longest_i
    | p :: ps ->
      match common p.label, longest_len with
      | Some common_len, Some longest_len when common_len > longest_len ->
        find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
      | Some common_len, None ->
        find_by_prefix ps ~i:(succ i) ~longest_len:common_len ~longest_i:i
      | _ ->
        find_by_prefix ps ~i:(succ i) ?longest_len ?longest_i
  in
  find_by_prefix params

let application_signature ~prefix = function
  (* provide signature information for applied functions *)
  | (_, Expression arg) :: (_, Expression { exp_desc =
      Texp_apply ({ exp_type = { desc = Tarrow _; _ }; _ } as e, args); _}) :: _ ->
    let result = separate_function_signature e ~args in
    let active_param = active_parameter_by_arg ~arg result.parameters in
    let active_param = match active_param with
      | Some _ as ap -> ap
      | None -> active_parameter_by_prefix ~prefix result.parameters
    in
    Some { result with active_param }

  (* provide signature information directly after an unapplied function-type
     value *)
  | (_, Expression ({ exp_type = { desc = Tarrow _; _ }; _ } as e)) :: _ ->
    let result = separate_function_signature e ~args:[] in
    let active_param = active_parameter_by_prefix ~prefix result.parameters in
    Some { result with active_param }

  | _ -> None
