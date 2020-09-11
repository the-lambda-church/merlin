(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>
                             Jeremie Dimino  <jeremie(_)dimino.org>

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

open Browse_raw

open Extend_protocol.Reader

type raw_info =
  [ `Constructor of Types.constructor_description
  | `Modtype of Types.module_type
  | `Modtype_declaration of Ident.t * Types.modtype_declaration
  | `None
  | `String of string
  | `Type_declaration of Ident.t * Types.type_declaration
  | `Type_scheme of Types.type_expr
  | `Variant of string * Types.type_expr option
  ]

let raw_info_printer : raw_info -> _ = function
  | `Constructor c ->
    `Print (Out_type (Browse_misc.print_constructor c))
  | `Modtype mt ->
    `Print (Out_module_type (Printtyp.tree_of_modtype mt))
  | `Modtype_declaration (id, mtd) ->
    `Print (Out_sig_item
              (Printtyp.tree_of_modtype_declaration id mtd))
  | `None -> `String ""
  | `String s -> `String s
  | `Type_declaration (id, tdecl) ->
    `Print (Out_sig_item
              (Printtyp.tree_of_type_declaration id tdecl Types.Trec_first))
  | `Type_scheme te ->
    `Print (Out_type (Printtyp.tree_of_type_scheme te))
  | `Variant (label, arg) ->
    begin match arg with
      | None -> `String label
      | Some te ->
        `Concat (label ^ " of ",
                 Out_type (Printtyp.tree_of_type_scheme te))
    end

(* List methods of an object.
   Code taken from [uTop](https://github.com/diml/utop
   with permission from Jeremie Dimino. *)
let lookup_env f x env =
  try Some (f x env)
  with Not_found | Env.Error _ -> None

let parenthesize_name name =
  (* Qualified operators need parentheses *)
  if name = "" || not (Oprint.parenthesized_ident name) then name else (
    if name.[0] = '*' || name.[String.length name - 1] = '*' then
      "( " ^ name ^ " )"
    else
      "(" ^ name ^ ")"
  )

let rec methods_of_type env ?(acc=[]) type_expr =
  let open Types in
  match type_expr.desc with
  | Tlink type_expr | Tobject (type_expr, _) | Tpoly (type_expr, _) ->
    methods_of_type env ~acc type_expr
  | Tfield (name, _, ty, rest) ->
    methods_of_type env ~acc:((name,ty) :: acc) rest
  | Tconstr (path, _, _) -> begin
      match lookup_env Env.find_type path env with
      | None | Some { type_manifest = None; _ } -> acc
      | Some { type_manifest = Some type_expr; _ } ->
        methods_of_type env ~acc type_expr
    end
  | _ -> acc

let classify_node = function
  | Dummy                      -> `Expression
  | Pattern                  _ -> `Pattern
  | Expression               _ -> `Expression
  | Case                     _ -> `Pattern
  | Class_expr               _ -> `Expression
  | Class_structure          _ -> `Expression
  | Class_field              _ -> `Expression
  | Class_field_kind         _ -> `Expression
  | Module_expr              _ -> `Module
  | Module_type_constraint   _ -> `Module_type
  | Structure                _ -> `Structure
  | Structure_item           _ -> `Structure
  | Module_binding           _ -> `Module
  | Value_binding            _ -> `Type
  | Module_type              _ -> `Module_type
  | Signature                _ -> `Signature
  | Signature_item           _ -> `Signature
  | Module_declaration       _ -> `Module
  | Module_type_declaration  _ -> `Module_type
  | With_constraint          _ -> `Type
  | Core_type                _ -> `Type
  | Package_type             _ -> `Module_type
  | Row_field                _ -> `Expression
  | Value_description        _ -> `Type
  | Type_declaration         _ -> `Type
  | Type_kind                _ -> `Type
  | Type_extension           _ -> `Type
  | Extension_constructor    _ -> `Type
  | Label_declaration        _ -> `Type
  | Constructor_declaration  _ -> `Type
  | Class_type               _ -> `Type
  | Class_signature          _ -> `Type
  | Class_type_field         _ -> `Type
  | Class_declaration        _ -> `Expression
  | Class_description        _ -> `Type
  | Class_type_declaration   _ -> `Type
  | Method_call              _ -> `Expression
  | Record_field             (`Expression _, _, _) -> `Expression
  | Record_field             (`Pattern _, _, _) -> `Pattern
  | Module_binding_name      _ -> `Module
  | Module_declaration_name  _ -> `Module
  | Module_type_declaration_name _ -> `Module_type
  | Open_description _ -> `Module
  | Open_declaration _ -> `Module
  | Include_declaration _ -> `Module
  | Include_description _ -> `Module

open Query_protocol.Compl

let map_entry f entry =
  {entry with desc = f entry.desc; info = f entry.info}

let make_candidate ~get_doc ~attrs ~exact ~prefix_path name ?loc ?path ty =
  let ident = match path with
    | Some path ->
      (* this is not correct: the ident is not persistent, the printing of some
         polymorphic variant type could (perhaps) be incorrect because of this
         (though I haven't tried to add a test). But it would be incorrect with
         any ident with synthesize at this point anyway.
         And create_persistent is the only function which is available on all
         the versions of ocaml we support. *)
      Ident.create_persistent (Path.last path)
    | None -> Extension.ident
  in
  let kind, text =
    match ty with
    | `Value v ->
      (`Value, `Type_scheme v.Types.val_type)
    | `Cons c  -> (`Constructor, `Constructor c)
    | `Label label_descr ->
      let desc =
        Types.(Tarrow (Ast_helper.no_label,
                       label_descr.lbl_res, label_descr.lbl_arg, Cok))
      in
      (`Label, `Type_scheme (Btype.newgenty desc))
    | `Label_decl (ty,label_decl) ->
      let desc =
        Types.(Tarrow (Ast_helper.no_label,
                       ty, label_decl.ld_type, Cok))
      in
      (`Label, `Type_scheme (Btype.newgenty desc))
    | `Mod m   ->
      begin try
          if not exact then raise Exit;
          let verbosity = !Type_utils.verbosity in
          if Type_utils.mod_smallerthan (1000 * verbosity) m = None then raise Exit;
          (`Module, `Modtype m)
        with Exit -> (`Module, `None)
      end
    | `ModType m ->
      if exact then
        (`Modtype, `Modtype_declaration (ident, (*verbose_sig env*) m))
      else
        (`Modtype, `None)
    | `Typ t ->
      (`Type, `Type_declaration (ident, t))
    | `Variant (label,arg) ->
      (`Variant, `Variant (label, arg))
  in
  (* FIXME: When suggesting variants (and constructors) with parameters,
     it could be nice to check precedence and add or not parenthesis.
  let name = match ty with
    | `Variant (_, Some _) -> "(" ^ name ^ " )"
    | _ -> name
  in*)
  let name =
    match prefix_path with
    | None -> name
    | Some _ -> parenthesize_name name
  in
  let desc =
    match kind with
    | `Module | `Modtype -> `None
    | _ -> text
  in
  let info = match Type_utils.read_doc_attributes attrs, get_doc, kind with
    | Some (str, _), _, _ -> `String str
    | None, _, (`Module | `Modtype) -> text
    | None, None, _ -> `None
    | None, Some get_doc, kind ->
      match path, loc with
      | Some p, Some loc ->
        let namespace = (* FIXME: that's just terrible *)
          match kind with
          | `Value -> `Vals
          | `Type -> `Type
          | _ -> assert false
        in
        begin match get_doc (`Completion_entry (namespace, p, loc)) with
          | `Found str -> `String str
          | _ -> `None
          | exception _ -> `None
        end
      | _, _ -> `None
  in
  let deprecated = Type_utils.is_deprecated attrs in
  {name; kind; desc; info; deprecated}

let item_for_global_module name =
  {name; kind = `Module; desc = `None; info = `None; deprecated = false}

let fold_constructors f id env acc =
  Env.fold_constructors
    (fun constr acc -> f constr.Types.cstr_name constr acc)
    id env acc

let fold_variant_constructors ~env ~init ~f =
  let rec aux acc t =
    let t = Ctype.repr t in
    match t.Types.desc with
    | Types.Tvariant { Types. row_fields; row_more; _ } ->
      let acc =
        let keep_if_present acc (lbl, row_field) =
          match row_field with
          | Types.Rpresent arg when lbl <> "" -> f ("`" ^ lbl) arg acc
          | Types.Reither (_, lst, _, _) when lbl <> "" ->
            let arg =
              match lst with
              | [ well_typed ] -> Some well_typed
              | _ -> None
            in
            f ("`" ^ lbl) arg acc
          | _ -> acc
        in
        List.fold_left ~init:acc row_fields ~f:keep_if_present
      in
      aux acc row_more
    | Types.Tconstr _ ->
      let t' = try Ctype.full_expand env t with _ -> t in
      if Types.TypeOps.equal t t' then
        acc
      else
        aux acc t'
    | _ -> acc
  in
  aux init

let get_candidates ?get_doc ?target_type ?prefix_path ~prefix kind ~validate env branch =
  let cstr_attributes c = c.Types.cstr_attributes in
  let val_attributes v = v.Types.val_attributes in
  let type_attributes t = t.Types.type_attributes in
  let lbl_attributes l = l.Types.lbl_attributes in
  let mtd_attributes t = t.Types.mtd_attributes in
  let md_attributes t = t.Types.md_attributes in
  let make_candidate ~attrs ~exact name ?loc ?path ty =
    make_candidate ~get_doc ~prefix_path ~attrs ~exact name ?loc ?path ty in
  let make_weighted_candidate ?(priority=0) ~attrs ~exact name ?loc ?path ty =
    (* Just like [make_candidate] but associates some metadata to the candidate.
       The candidates are later sorted using these metadata.

       The ordering works as follow:
       - first we compare the priority of the candidates
       - we compare the cost of unification for both (using Btype.total_changes)
       - if they are equal, then we compare their "binding time": things
         introduced more recently will come before older bindings (i.e. we
         prioritize the local context)
       - if these are also equal, then we just use classic string ordering on
         the candidate name. *)
    let time =
      try Path.scope (Option.get path)
      with _ -> 0
    in
    let item = make_candidate ~attrs ~exact name ?loc ?path ty in
    (- priority, - time, name), item
  in
  let is_internal name = name = "" || name.[0] = '_' in
  let items =
    let snap = Btype.snapshot () in
    let rec arrow_arity n t =
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> arrow_arity (n + 1) rhs
      | _ -> n
    in
    let rec nth_arrow n t =
      if n <= 0 then t else
      match (Ctype.repr t).Types.desc with
      | Types.Tarrow (_,_,rhs,_) -> nth_arrow (n - 1) rhs
      | _ -> t
    in
    let type_check =
      (* Defines the priority of a candidate.
         Priority is 1000 - cost - head_arrows, where:
         - cost is the number unification variables instantiated to make the types unify
         - head_arrows is 0 if types unified, or the number of arrows which
           have been skipped to make them unify (i.e types would unify if the
           user apply the function to head_arrows arguments).
         Note that if no type is expected (context was not inferred), 0 will be
         returned. *)
      match target_type with
      | None -> fun _ -> 0
      | Some ty ->
        let arity = arrow_arity 0 ty in
        fun scheme ->
        let cost =
          let c = Btype.linked_variables in
          try
            let c' = c () in
            Ctype.unify_var env ty (Raw_compat.ctype_instance env scheme);
            c () - c'
          with _ ->
            let arity = arrow_arity (-arity) scheme in
            if arity > 0 then begin
              let c' = c () in
              Btype.backtrack snap;
              let ty' = Raw_compat.ctype_instance env scheme in
              let ty' = nth_arrow arity ty' in
              try Ctype.unify_var env ty ty'; arity + c () - c'
              with _ -> 1000
            end
            else 1000
        in
        Btype.backtrack snap;
        1000 - cost
    in
    let of_kind = function
      | `Variants ->
        let add_variant name param candidates =
          if not @@ validate `Variant `Variant name then candidates else
            make_weighted_candidate name ~exact:false ~priority:2 ~attrs:[]
              (`Variant (name, param))
            :: candidates
        in
        let result = match target_type with
          | None -> []
          | Some t -> fold_variant_constructors t ~init:[] ~f:add_variant ~env
        in
        let result = match branch with
          | _ :: (_, Expression {Typedtree. exp_type = t; _}) :: _
          | (_, Expression {Typedtree. exp_type = t; _}) :: _ ->
            fold_variant_constructors t ~init:result ~f:add_variant ~env
          | _ -> result
        in
        result
      | `Values ->
        let type_check {Types. val_type; _} = type_check val_type in
        Env.fold_values (fun name path v candidates ->
          if not (validate `Lident `Value name) then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name = prefix) name ~priority ~path
            ~attrs:(val_attributes v)
            (`Value v) ~loc:v.Types.val_loc
          :: candidates
        ) prefix_path env []

      | `Constructor ->
        let type_check {Types. cstr_res; _} = type_check cstr_res in
        fold_constructors (fun name v candidates ->
          if not @@ validate `Lident `Cons name then candidates else
          let priority = if is_internal name then 0 else type_check v in
          make_weighted_candidate ~exact:(name=prefix) name (`Cons v) ~priority
            ~attrs:(cstr_attributes v)
          :: candidates
        ) prefix_path env []

      | `Types ->
        Env.fold_type_decls (fun name path decl candidates ->
          if not @@ validate `Lident `Typ name then candidates else
          make_weighted_candidate ~exact:(name = prefix) name ~path (`Typ decl)
            ~loc:decl.Types.type_loc ~attrs:(type_attributes decl)
          :: candidates
        ) prefix_path env []

      | `Modules ->
        Env.fold_modules (fun name path v candidates ->
          let attrs = md_attributes v in
          let v = v.Types.md_type in
          if not @@ validate `Uident `Mod name then candidates else
            make_weighted_candidate ~exact:(name = prefix) name ~path (`Mod v) ~attrs
          :: candidates
        ) prefix_path env []

      | `Modules_type ->
        Env.fold_modtypes (fun name path v candidates ->
          if not @@ validate `Uident `Mod name then candidates else
            make_weighted_candidate ~exact:(name=prefix) name ~path (`ModType v)
              ~attrs:(mtd_attributes v)
            :: candidates
        ) prefix_path env []

      | `Labels ->
        Env.fold_labels (fun ({Types.lbl_name = name; _} as l) candidates ->
          if not (validate `Lident `Label name) then candidates else
            make_weighted_candidate ~exact:(name = prefix) name (`Label l)
              ~attrs:(lbl_attributes l)
            :: candidates
        ) prefix_path env []
    in
    let of_kind_group = function
      | #Query_protocol.Compl.kind as k -> of_kind k
      | `Group kinds -> List.concat_map ~f:of_kind kinds
    in
    try of_kind_group kind
    with exn ->
      Logger.log ~section:"Completion" ~title:"get_candidates/of_kind"
        "Failed with exception: %a" Logger.exn exn;
      []
  in
  let items = List.sort items ~cmp:(fun (a,_) (b,_) -> compare a b) in
  let items = List.rev_map ~f:snd items in
  items

let gen_values = `Group [`Values; `Constructor]

let default_kinds = [`Variants; gen_values; `Types; `Modules; `Modules_type]

let completion_order = function
  | `Expression  -> [`Variants; gen_values; `Types; `Modules; `Modules_type]
  | `Structure   -> [gen_values; `Types; `Modules; `Modules_type]
  | `Pattern     -> [`Variants; `Constructor; `Modules; `Labels; `Values; `Types; `Modules_type]
  | `Module      -> [`Modules; `Modules_type; `Types; gen_values]
  | `Module_type -> [`Modules_type; `Modules; `Types; gen_values]
  | `Signature   -> [`Types; `Modules; `Modules_type; gen_values]
  | `Type        -> [`Types; `Modules; `Modules_type; gen_values]

type kinds = [kind | `Group of kind list] list

let complete_methods ~env ~prefix obj =
  let t = obj.Typedtree.exp_type in
  let has_prefix (name,_) =
    String.is_prefixed ~by:prefix name &&
    (* Prevent identifiers introduced by type checker to leak *)
    try ignore (String.index name ' ' : int); false
    with Not_found -> true
  in
  let methods = List.filter ~f:has_prefix (methods_of_type env t) in
  List.map methods ~f:(fun (name,ty) ->
    let info = `None (* TODO: get documentation. *) in
    { name; kind = `MethodCall; desc = `Type_scheme ty; info; deprecated = false }
  )

type is_label =
  [ `No | `Maybe
  | `Description of Types.label_description list
  | `Declaration of Types.type_expr * Types.label_declaration list
  ]

let complete_prefix ?get_doc ?target_type ?(kinds=[]) ~prefix ~is_label
    config (env,node) branch =
  Env.with_cmis @@ fun () ->
  let seen = Hashtbl.create 7 in
  let uniq n = if Hashtbl.mem seen n
    then false
    else (Hashtbl.add seen n (); true)
  in
  let make_candidate ~attrs ~exact name ?loc ?path ty =
    make_candidate ~get_doc ~attrs ~exact name ?loc ?path ty in
  let find ?prefix_path ~is_label prefix =
    let valid tag name =
      let no_leak () =
        (* Prevent identifiers introduced by type checker
          and recovery to leak *)
        List.for_all ~f:(fun by -> not (String.is_prefixed ~by name))
          ["self-"; "selfpat-"; "*type-"]
      in
      String.is_prefixed ~by:prefix name
      && uniq (tag,name)
      && no_leak ()
    in
    (* Hack to prevent extensions namespace to leak
       + another to hide the "Library_name__Module" present at Jane Street *)
    let validate ident tag name =
      (if ident = `Uident
       then name <> "" && name.[0] <> '_'
            && (String.no_double_underscore name || tag <> `Mod)
       else name <> "_")
      && valid tag name
    in
    let add_label_description ({Types.lbl_name = name; _} as l) candidates =
      if not (valid `Label name) then candidates else
        make_candidate ~prefix_path ~exact:(name = prefix) name
          (`Label l) ~attrs:[]
        :: candidates
    in
    let add_label_declaration ty ({Types.ld_id = name; _} as l) candidates =
      let name = Ident.name name in
      if not (valid `Label name) then candidates else
        make_candidate ~prefix_path ~exact:(name = prefix) name
          (`Label_decl (ty,l)) ~attrs:[]
        :: candidates
    in
    let base_completion = match (is_label : is_label) with
      | `No -> []
      | `Maybe ->
        Env.fold_labels add_label_description prefix_path env []
      | `Description lbls ->
        List.fold_right ~f:add_label_description lbls ~init:[]
      | `Declaration (ty,decls) ->
        List.fold_right ~f:(add_label_declaration ty) decls ~init:[]
    in
    if base_completion = [] then
      let order =
        if kinds = [] then
          let kind = classify_node node in
          completion_order kind
        else
          (kinds : kind list :> kinds)
      in
      let add_completions acc kind =
        get_candidates ?get_doc ?target_type ?prefix_path ~prefix kind ~validate env branch @ acc
      in
      List.fold_left ~f:add_completions order ~init:[]
    else base_completion
  in
  try
    match prefix with
    | Longident.Ldot (prefix_path, prefix) -> find ~prefix_path ~is_label prefix
    | Longident.Lident prefix ->
      let compl = find ~is_label prefix in
      (* Add modules on path but not loaded *)
      List.fold_left (Mconfig.global_modules config) ~init:compl ~f:(
        fun candidates name ->
          if not (String.no_double_underscore name) then candidates else
          let default =
            { name; kind = `Module; desc = `None; info = `None; deprecated = false } in
          if name = prefix && uniq (`Mod, name) then
            try
              let path, md, attrs = Type_utils.lookup_module (Longident.Lident name) env in
              make_candidate ~prefix_path:(Some prefix) ~exact:true ~path name
                 (`Mod md) ~attrs
              :: candidates
            with Not_found ->
              default :: candidates
          else if String.is_prefixed ~by:prefix name && uniq (`Mod,name) then
            default :: candidates
          else
            candidates
      )
    | _ -> find ~is_label (String.concat ~sep:"." @@ Longident.flatten prefix)
  with Not_found -> []

(* Propose completion from a particular node *)
let branch_complete buffer ?get_doc ?target_type ?kinds prefix = function
  | [] -> []
  | (env, node) :: branch ->
    match node with
    | Method_call (obj,_,_) -> complete_methods ~env ~prefix obj
    | Pattern    { Typedtree.pat_desc = Typedtree.Tpat_record _ ; pat_type = t ; _ }
    | Expression { Typedtree.exp_desc = Typedtree.Texp_record _ ; exp_type = t ; _ } ->
      let is_label =
        try match t.Types.desc with
          | Types.Tconstr (p, _, _) ->
            (match (Env.find_type p env).Types.type_kind with
             | Types.Type_record (labels, _) ->
               `Declaration (t, labels)
             | _ -> `Maybe)
          | _ -> `Maybe
        with _ -> `Maybe
      in
      let prefix, _is_label = Longident.(keep_suffix @@ parse prefix) in
      complete_prefix ?get_doc ?target_type ?kinds ~prefix ~is_label buffer
        (env,node) branch
    | Record_field (parent, lbl, _) ->
      let prefix, _is_label = Longident.(keep_suffix @@ parse prefix) in
      let snap = Btype.snapshot () in
      let is_label = match lbl.Types.lbl_all with
        | [||] ->
          begin match
              let ty = match parent with
                | `Expression e -> e.Typedtree.exp_type
                | `Pattern p -> p.Typedtree.pat_type
              in
              let p, _, decl = Ctype.extract_concrete_typedecl env ty in
              (ty, p, decl)
            with
            | exception _ -> `Maybe
            | (ty, p, decl) ->
              try
                let lbls = Datarepr.labels_of_type p decl in
                let labels = List.map lbls ~f:(fun (_,lbl) ->
                    try
                      let _, lbl_arg, lbl_res = Ctype.instance_label false lbl in
                      begin try
                          Ctype.unify_var env ty lbl_res;
                        with _ -> ()
                      end;
                      (* FIXME: the two subst can lose some sharing between types *)
                      let lbl_res = Subst.type_expr Subst.identity lbl_res in
                      let lbl_arg = Subst.type_expr Subst.identity lbl_arg in
                      {lbl with Types. lbl_res; lbl_arg}
                    with _ -> lbl
                  ) in
                `Description labels
              with _ ->
              match decl.Types.type_kind with
              | Types.Type_record (lbls, _) ->
                `Declaration (ty, lbls)
              | _ -> `Maybe
          end
        | lbls ->
          `Description (Array.to_list lbls)
      in
      let result =
        complete_prefix ?get_doc ?target_type ?kinds ~prefix ~is_label buffer
          (env, node) branch
      in
      Btype.backtrack snap;
      result
    | _ ->
      let prefix, is_label = Longident.(keep_suffix @@ parse prefix) in
      complete_prefix ?get_doc ?target_type ?kinds ~prefix buffer
        ~is_label:(if is_label then `Maybe else `No)
        (env, node) branch

let expand_prefix ~global_modules ?(kinds=[]) env prefix =
  Env.with_cmis @@ fun () ->
  let lidents, last =
    let ts = Expansion.explore ~global_modules env in
    Expansion.get_lidents ts prefix
  in
  let validate' =
    let last = Expansion.spell_index last in
    fun s -> Expansion.spell_match last s
  in
  let validate _ _ s = validate' s in
  let kinds = match kinds with
    | [] -> default_kinds
    | kinds -> (kinds : kind list :> kinds)
  in
  let process_prefix_path prefix_path =
    let candidates =
      let aux compl kind =
        get_candidates ?prefix_path ~prefix:"" kind ~validate env [] @ compl in
      List.fold_left ~f:aux kinds ~init:[]
    in
    match prefix_path with
    | None ->
      let f name =
        if not (validate' name) then None else
          Some (item_for_global_module name)
      in
      candidates @ List.filter_map global_modules ~f
    | Some lident ->
      let lident = Longident.flatten lident in
      let lident = String.concat ~sep:"." lident ^ "." in
      List.map candidates ~f:(fun c -> { c with name = lident ^ parenthesize_name c.name })
  in
  List.concat_map ~f:process_prefix_path lidents

open Typedtree

let application_context ~prefix path =
  let module Printtyp = Type_utils.Printtyp in
  let target_type = ref (
      match snd (List.hd path) with
      | Expression { exp_type = ty ; _ }
      | Pattern    { pat_type = ty ; _ } -> Some ty
      | _ -> None
    )
  in
  let context = match path with
    | (_, Expression earg) ::
      (_, Expression ({ exp_desc = Texp_apply (efun, _); _ } as app)) :: _
      when earg != efun ->
      (* Type variables shared across arguments should all be
         printed with the same name.
         [Printtyp.type_scheme] ensure that a name is unique within a given
         type, but not across different invocations.
         [reset] followed by calls to [mark_loops] and [type_sch] provide
         that *)
      Printtyp.reset ();
      let pr t =
        let ppf, to_string = Format.to_string () in
        Printtyp.mark_loops t;
        Printtyp.type_sch ppf t;
        to_string ()
      in
      (* Special case for optional arguments applied with ~,
         get the argument wrapped inside Some _ *)
      let earg =
        match Mbrowse.optional_label_sugar earg.exp_desc with
        | None -> earg
        | Some earg ->
          target_type := Some earg.exp_type;
          earg
      in
      let labels = Raw_compat.labels_of_application ~prefix app in
      `Application { argument_type = pr earg.exp_type;
                     labels = List.map ~f:(fun (lbl,ty) -> lbl, pr ty) labels;
                   }
    | _ -> `Unknown
  in
  !target_type, context
