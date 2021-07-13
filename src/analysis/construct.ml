open Std
open Typedtree

let {Logger. log} = Logger.for_section "construct"

type values_scope = Null | Local
type what = Modtype | Mod

exception Not_allowed of string
exception Not_a_hole
exception Modtype_not_found of what * string
exception No_constraint

let () =
  Location.register_error_of_exn (function
    | Not_a_hole ->
      Some (Location.error "Construct only works on holes.")
    | Modtype_not_found (Modtype, s) ->
      let txt = Format.sprintf "Module type not found: %s" s in
      Some (Location.error txt)
    | Modtype_not_found (Mod, s) ->
      let txt = Format.sprintf "Module not found: %s" s in
      Some (Location.error txt)
    | No_constraint ->
      Some (Location.error 
        "Could not find a module type to construct from. \
        Check that you used a correct constraint.")
    | _ -> None
  )
module Util = struct
  open Destruct.Path_utils
  open Types

  let predef_types =
    let tbl = Hashtbl.create 14 in
    let () =
      let constant c =
        Ast_helper.Exp.constant c
      in
      let construct s =
        Ast_helper.Exp.construct (Location.mknoloc (Longident.Lident s)) None
      in
      let ident s =
        Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident s))
      in
      List.iter ~f:(fun (k, v) -> Hashtbl.add tbl k v)
        Parsetree.[
          Predef.path_int, constant (Pconst_integer("0", None)) ;
          Predef.path_float, constant (Pconst_float("0.0", None)) ;
          Predef.path_char, constant (Pconst_char 'c') ;
          Predef.path_string,
            constant (Pconst_string("", Location.none, None)) ;
          Predef.path_bool, construct "true" ;
          Predef.path_unit, construct "()" ;
          Predef.path_exn, ident "exn" ;
          Predef.path_array, Ast_helper.Exp.array [] ;
          Predef.path_nativeint, constant (Pconst_integer("0", Some 'n')) ;
          Predef.path_int32, constant (Pconst_integer("0", Some 'l')) ;
          Predef.path_int64, constant (Pconst_integer("0", Some 'L')) ;
          Predef.path_lazy_t, Ast_helper.Exp.lazy_ (construct "()")
        ]
    in
    tbl

  let prefix env ~env_check path name = 
    to_shortest_lid ~env ~env_check ~name path

  let var_of_id id = Location.mknoloc @@ Ident.name id

  let type_to_string t =
    Printtyp.type_expr (Format.str_formatter) t;
    Format.flush_str_formatter ()

  let unifiable env type_expr type_expected = 
    let snap = Btype.snapshot () in
    try
      Ctype.unify env type_expected type_expr |> ignore; 
      Some snap
    with Ctype.Unify _  -> 
      (* Unification failure *) 
      Btype.backtrack snap;
      None
  
  let is_in_stdlib path =
    Path.head path |> Ident.name = "Stdlib" 

  (** [find_values_for_type env typ] searches the environment [env] for
  {i values} with a return type compatible with [typ] *)
  let find_values_for_type env typ =
    let aux name path value_description acc =
      (* [check_type| checks return type compatibility and lists parameters *)
      let rec check_type type_expr params =
        let type_expr = Btype.repr type_expr in
        (* TODO is this test general enough ? *)
        match unifiable env type_expr typ with
        | Some snap ->
          (* This will be called multiple times so we need to backtrack
              See c-simple, test 6.2b for an example *)
          Btype.backtrack snap;
          Some params
        | None -> 
          begin match type_expr.desc with
          | Tarrow (arg_label, _, te, _) -> check_type te (arg_label::params)
          | _ -> None
      end
      in
      (* TODO we should probably sort the results better *)
      match is_in_stdlib path, check_type value_description.val_type [] with
      | false, Some params ->
        Path.Map.add path (name, value_description, params) acc
      | _, _ -> acc
    in
    (* We look for values in the current scope and in local unonpend submodules.
      We also exclude the Stdlib modules from the search. *)
    let fold_values path acc = Env.fold_values aux path env acc in
    let init = fold_values None Path.Map.empty in
    Env.fold_modules (fun name path _module_decl acc ->
      if not (is_in_stdlib path) && not (is_opened env path) then 
        (* We ignore opened modules. That means that is a value of an opened
          module has been shadowed we won't suggest the one in the opened
          module. *) 
        fold_values (Some (Untypeast.lident_of_path path)) acc
      else acc) None env init
      

  (** The idents_table is used to keep track of already used names when
  generating function arguments in the same expression *)
  let idents_table ~keywords =
    let table = Hashtbl.create 50 in
    (* We add keywords to the table so they are always numbered *)
    List.iter keywords ~f:(fun k -> Hashtbl.add table k (-1));
    table

  (* Given a list [l] of n elements which are lists of choices,
    [combination l] is a list of all possible combinations of
    these choices (cartesian product). For example:

    let l = [["a";"b"];["1";"2"]; ["x"]];;
    combinations l;;
    - : string list list =
    [["a"; "1"; "x"]; ["b"; "1"; "x"];
     ["a"; "2"; "x"]; ["b"; "2"; "x"]]

    If the input is the empty list, the result is
    the empty list singleton list.
    *)
  let combinations l =
    List.fold_left l
      ~init:[[]]
      ~f:(fun acc_l choices_for_arg_i ->
        List.fold_left choices_for_arg_i
          ~init:[]
          ~f:(fun acc choice_arg_i ->
            let choices = List.map acc_l
              ~f:(fun l -> List.rev (choice_arg_i :: l))
            in
            List.rev_append acc choices))

  (** [panache2 l1 l2] returns a new list containing an interleaving of the
  values in [l1] and [l2] *)
  let panache2 l1 l2 =
    let rec aux acc l1 l2 =
      match l1, l2 with
      | [], [] -> List.rev acc
      | tl, [] | [], tl -> List.rev_append acc tl
      | a::tl1, b::tl2 -> aux (a::b::acc) tl1 tl2
  in aux [] l1 l2

  (* Given a list [l] of n lists, [panache l] flattens the list
    by starting with the first element of each, then the second one etc. *)
  let panache l =
    List.fold_left ~init:[] ~f:panache2 l
end

module Gen = struct
  open Types

  let hole = Ast_helper.Exp.hole ()

  (* [make_value] generates the PAST repr of a value applied to holes *)
  let make_value env (path, (name, value_description, params)) =
    let open Ast_helper in
    let env_check = Env.find_value_by_name in
    let lid = Location.mknoloc (Util.prefix env ~env_check path name) in
    let params = List.map params
      ~f:(fun label -> label, Exp.hole ())
    in
    if List.length params > 0 then
      Exp.(apply (ident lid) params)
    else Exp.ident lid

  (* We never perform deep search when constructing modules *)
  let rec module_ env =
    let open Ast_helper in function
    | Mty_ident path -> begin
      try
        let m = Env.find_modtype path env in
        match m.mtd_type with
        | Some t -> module_ env t
        | None -> raise Not_found
      with Not_found ->
        let name = Ident.name (Path.head path) in
        raise (Modtype_not_found (Modtype, name))
      end
    | Mty_signature sig_items ->
      let env = Env.add_signature sig_items env in
      Mod.structure @@ structure env sig_items
    | Mty_functor (param, out) ->
      let param = match param with
        | Unit -> Parsetree.Unit
        | Named (id, in_) ->
          Parsetree.Named (
            Location.mknoloc (Option.map ~f:Ident.name id),
            Ptyp_of_type.module_type in_)
      in
      Mod.functor_ param @@ module_ env out
    | Mty_alias path ->
      begin try let m = Env.find_module path env in
        module_ env m.md_type
        with Not_found ->
          let name = Ident.name (Path.head path) in
          raise (Modtype_not_found (Mod, name))
      end
    | Mty_for_hole -> Mod.mk Pmod_hole
  and structure_item env =
    let open Ast_helper in
    function
    | Sig_value (id, vd, _visibility) ->
      let vb = Vb.mk (Pat.var (Util.var_of_id id)) (Exp.hole ()) in
      Str.value Nonrecursive [ vb ]
    | Sig_type (id, type_declaration, rec_flag, _visibility) ->
      let td = Ptyp_of_type.type_declaration id type_declaration in
      let rec_flag = match rec_flag with
        | Trec_first | Trec_next -> Asttypes.Recursive
        | Trec_not -> Nonrecursive
      in (* mutually recursive types are really handled by [structure] *)
      Str.type_ rec_flag [td]
    | Sig_modtype (id, { mtd_type; _ }, _visibility) ->
      let mtd = Ast_helper.Mtd.mk
        ?typ:(Option.map ~f:Ptyp_of_type.module_type mtd_type)
        @@ Util.var_of_id id
      in
      Ast_helper.Str.modtype mtd
    | Sig_module (id, _, mod_decl, _, _) ->
      let module_binding =
        Ast_helper.Mb.mk
          (Location.mknoloc (Some (Ident.name id)))
          @@ module_ env mod_decl.md_type
      in
      Str.module_ module_binding
    | Sig_typext (id, ext_constructor, _, _) ->
      let lid =
        Untypeast.lident_of_path ext_constructor.ext_type_path
        |> Location.mknoloc
      in
      Str.type_extension @@ Ast_helper.Te.mk
        ~attrs:ext_constructor.ext_attributes
        ~params:[]
        ~priv:ext_constructor.ext_private
        lid
        [Ptyp_of_type.extension_constructor id ext_constructor]
    | Sig_class_type (id, class_type_decl, _, _) ->
      let str = Format.asprintf "Construct does not handle class types yet. \
      Please replace this comment by [%s]'s definition." (Ident.name id) in
      Str.text [ Docstrings.docstring str Location.none ] |> List.hd
    | Sig_class (id, class_decl, _, _) ->
      let str = Format.asprintf "Construct does not handle classes yet. \
      Please replace this comment by [%s]'s definition." (Ident.name id) in
      Str.text [ Docstrings.docstring str Location.none ] |> List.hd
  and structure env (items : Types.signature_item list) =
    List.map (Ptyp_of_type.group_items items) ~f:(function
      | Ptyp_of_type.Item item -> structure_item env item
      | Ptyp_of_type.Type (rec_flag, type_decls) ->
        Ast_helper.Str.type_ rec_flag type_decls)

  (* [expression values_scope ~depth env ty] generates a list of PAST
    expressions that could fill a hole of type [ty] in the environment [env].
    [depth] regulates the deep construction of recursive values. If
    [values_scope] is set to [Local] the returned list will also contains
    local values to choose from *)
  let rec expression ~idents_table values_scope ~depth =
    let exp_or_hole env typ =
      if depth > 1 then
        (* If max_depth has not been reached we recurse *)
        expression ~idents_table values_scope ~depth:(depth - 1) env typ
      else
        (* else we return a hole *)
        [ Ast_helper.Exp.hole () ]
    in
    let arrow_rhs env typ =
      match (Ctype.repr typ).desc with
      | Tarrow _ -> expression ~idents_table values_scope ~depth env typ
      | _ -> exp_or_hole env typ
    in

    (* [make_arg] tries to provide a nice default name for function args *)
    let make_arg =
      let make_i n i =
        Hashtbl.replace idents_table n i;
        Printf.sprintf "%s_%i" n i
      in
      let uniq_name env n =
        let id = Ident.create_local n in
        try
          let i = Hashtbl.find idents_table n + 1 in
          make_i n i
        with Not_found ->
          try
            let _ = Env.find_value (Path.Pident id) env in
            make_i n 0
          with Not_found -> Hashtbl.add idents_table n 0; n
      in
      fun env label ty ->
        let open Asttypes in
        match label with
        | Labelled s | Optional s ->
            (* Pun for labelled arguments *)
            Ast_helper.Pat.var ( Location.mknoloc s), s
        | Nolabel -> begin match ty.desc with
          | Tconstr (path, _, _) ->
            let name = uniq_name env (Path.last path) in
            Ast_helper.Pat.var (Location.mknoloc name), name
          | _ -> Ast_helper.Pat.any (), "_" end
    in

    let constructor env type_expr path constrs =
      log ~title:"constructors" "[%s]"
        (String.concat ~sep:"; "
          (List.map constrs ~f:(fun c -> c.Types.cstr_name)));
      (* [make_constr] builds the PAST repr of a type constructor applied
      to holes *)
      let make_constr env path type_expr cstr_descr =
        let ty_args, ty_res = Ctype.instance_constructor cstr_descr in
        match Util.unifiable env type_expr ty_res with
        | Some snap ->
          let lid =
            Util.prefix env ~env_check:Env.find_constructor_by_name
              path cstr_descr.cstr_name
            |> Location.mknoloc
          in
          let args = List.map ty_args ~f:(exp_or_hole env) in
          let args_combinations = Util.combinations args in
          let exps = List.map args_combinations
            ~f:(function
              | [] -> None
              | [e] -> Some e
              | l -> Some (Ast_helper.Exp.tuple l))
          in
          Btype.backtrack snap;
          List.filter_map exps
            ~f:(fun exp ->
              let exp = Ast_helper.Exp.construct lid exp in
              (* For gadts not all combinations will be valid.
                See Test 6.1b in c-simple.t for an example.

                We therefore check that constructed expressions
                can be typed. *)
              try
                Typecore.type_expression env exp |> ignore;
                Some exp
              with _ -> None)
        | None -> []
      in
      List.map constrs ~f:(make_constr env path type_expr)
      |> Util.panache
    in

    let variant env typ row_desc =
      let fields =
        List.filter
          ~f:(fun (lbl, row_field) -> match row_field with
            | Rpresent _
            | Reither (true, [], _, _)
            | Reither (false, [_], _, _) -> true
            | _ -> false)
          row_desc.row_fields
      in
      match fields with
      | [] -> raise (Not_allowed "empty variant type")
      | row_descrs ->
        List.map row_descrs ~f:(fun (lbl, row_field) ->
          (match row_field with
            | Reither (false, [ty], _, _) | Rpresent (Some ty) ->
              List.map ~f:(fun s -> Some s) (exp_or_hole env ty)
            | _ -> [None])
            |> List.map ~f:(fun e ->
              Ast_helper.Exp.variant lbl e)
            )
        |> List.flatten
    in

    let record env typ path labels =
      log ~title:"record labels" "[%s]"
        (String.concat ~sep:"; "
          (List.map labels ~f:(fun l -> l.Types.lbl_name)));

      let labels = List.map labels ~f:(fun ({ lbl_name; _ } as lbl) ->
        let _, arg, res = Ctype.instance_label true lbl in
        Ctype.unify env res typ ;
        let lid =
          Util.prefix env ~env_check:Env.find_label_by_name path lbl_name
          |> Location.mknoloc
        in
        let exprs = exp_or_hole env arg in
        lid, exprs)
      in

      let lbl_lids, lbl_exprs = List.split labels in
      Util.combinations lbl_exprs
      |> List.map
          ~f:(fun lbl_exprs ->
            let labels = List.map2 lbl_lids lbl_exprs
              ~f:(fun lid exp -> (lid, exp))
            in
            Ast_helper.Exp.record labels None)
    in

    (* Given a typed hole, there is two possible forms of constructions:
      - Use the type's definition to propose the correct type constructors,
      - Look for values in the environnement with compatible return type. *)
    fun env typ ->
      log ~title:"construct expr" "Looking for expressions of type %s"
        (Util.type_to_string typ);
      let rtyp = Ctype.full_expand env typ |> Btype.repr in
      let constructed_from_type = match rtyp.desc with
        | Tlink _ | Tsubst _ ->
          assert false
        | Tpoly (texp, _)  ->
          (* We are not going "deeper" so we don't call [exp_or_hole] here *)
          expression ~idents_table values_scope ~depth env texp
        | Tunivar _ | Tvar _ ->
          [ ]
        | Tconstr (path, [texp], _) when path = Predef.path_lazy_t ->
          (* Special case for lazy *)
          let exps = exp_or_hole env texp in
          List.map exps ~f:Ast_helper.Exp.lazy_
        | Tconstr (path, params, _) ->
          (* If this is a "basic" type we propose a default value *)
          begin try
            [ Hashtbl.find Util.predef_types path ]
          with Not_found ->
            let def = Env.find_type_descrs path env in
            match def with
            | constrs, [] -> constructor env rtyp path constrs
            | [], labels -> record env rtyp path labels
            | _ -> []
          end
        | Tarrow (label, tyleft, tyright, _) ->
          let argument, name = make_arg env label tyleft in
          let value_description = {
              val_type = tyleft;
              val_kind = Val_reg;
              val_loc = Location.none;
              val_attributes = [];
              val_uid = Uid.mk ~current_unit:(Env.get_unit_name ());
            }
          in
          let env = Env.add_value (Ident.create_local name) value_description env in
          let exps = arrow_rhs env tyright in
          List.map exps ~f:(Ast_helper.Exp.fun_ label None argument)
        | Ttuple types ->
          let choices = List.map types ~f:(exp_or_hole env)
            |> Util.combinations
          in
          List.map choices  ~f:Ast_helper.Exp.tuple
        | Tvariant row_desc -> variant env rtyp row_desc
        | Tpackage (path, lids, args) -> begin
          let open Ast_helper in
          try
            let ty =
              Typemod.modtype_of_package env Location.none path lids args
            in
            let ast =
              Exp.constraint_
                (Exp.pack (module_ env ty))
                (Ptyp_of_type.core_type typ)
            in
            [ ast ]
          with Typemod.Error _ ->
            let name = Ident.name (Path.head path) in
            raise (Modtype_not_found (Modtype, name)) end
        | Tobject (fields, _) ->
          let rec aux acc fields =
            match fields.desc with
            | Tnil -> acc
            | Tvar _ | Tunivar _ -> acc
            | Tfield ("*dummy method*", _, _, fields) -> aux acc fields
            | Tfield (name, _, type_expr, fields) ->
              let exprs = exp_or_hole env type_expr
                |> List.map ~f:(fun expr ->
                  let open Ast_helper in
                  Cf.method_ (Location.mknoloc name) Asttypes.Public
                  @@ Ast_helper.Cf.concrete Asttypes.Fresh expr)
              in
              aux (exprs :: acc) fields
            | _ ->
              failwith @@ Format.asprintf
                "Unexpected type constructor in fields list: %a"
                Printtyp.type_expr fields
          in
          let all_fields = aux [] fields |> Util.combinations in
          List.map all_fields ~f:(fun fields ->
            let open Ast_helper in
            Exp.object_ @@ Ast_helper.Cstr.mk (Pat.any ()) fields)
        | Tfield _ | Tnil -> failwith "Found a field type outside an object"
      in
      let matching_values =
        if values_scope = Local then
          Path.Map.bindings (Util.find_values_for_type env typ)
          |> List.map ~f:(make_value env)
        else []
      in
      List.append constructed_from_type matching_values
end

let needs_parentheses e = match e.Parsetree.pexp_desc with
  | Pexp_fun _
  | Pexp_lazy _
  | Pexp_apply _
  | Pexp_variant (_, Some _)
  | Pexp_construct (_, Some _)
  -> true
  | _ -> false

let to_string_with_parentheses exp = 
  let f : _ format6 = 
    if needs_parentheses exp then "(%a)"
    else "%a"
  in
  Format.asprintf f Pprintast.expression exp

let node ?(depth = 1) ~keywords ~values_scope node =
  match node with
  | Browse_raw.Expression { exp_type; exp_env; _ } ->
      let idents_table = Util.idents_table ~keywords in
      Gen.expression ~idents_table values_scope ~depth exp_env exp_type
      |> List.map ~f:to_string_with_parentheses
  | Browse_raw.Module_expr 
      { mod_desc = Tmod_constraint _ ; mod_type; mod_env; _ }
  | Browse_raw.Module_expr 
      {  mod_desc = Tmod_apply _; mod_type; mod_env; _ } ->
      let m = Gen.module_ mod_env mod_type in
      [ Format.asprintf "%a" Pprintast.module_ m ]
  | Browse_raw.Module_expr _
  | Browse_raw.Module_binding _ -> 
      (* Constructible modules have an explicit constraint or are functor
        applications. In other cases we do not know what to construct.  *)
      raise No_constraint
  | _ -> raise Not_a_hole
