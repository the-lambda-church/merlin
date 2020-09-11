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
open Browse_tree
open Browse_raw

let {Logger. log} = Logger.for_section "typedtrie"

(* That's probably overkill, using a list would probably be just fine *)
module StampMap = Map.Make(struct
    type t = int
    let compare (x : int) (y: int) = compare x y
  end)

module Trie : sig
  [@@@ocaml.warning "-30"]

  type t

  and elt =
    { loc : Location.t
    ; doc : string option
    ; namespace : Namespaced_path.Namespace.t
    ; node : node }

  and node =
    | Leaf
    | Internal of t Lazy.t
    | Included of include_
    | Alias    of Namespaced_path.t
    | Functor  of functor_parameter * node
    | Apply    of functor_application

  and include_ =
    | Named of Namespaced_path.t
    | Items of t Lazy.t
    | Apply of functor_application

  and functor_parameter =
    (Ident.t option * Location.t * node) option

  and functor_application =
    { funct : Location.t * functor_
    ; arg   : Location.t * node }
    (* The day where we want to support aliasing of functor arguments then arg
       should be [elt] instead of [node] *)

  and functor_ =
    | Apply of functor_application
    | Funct of functor_parameter * node
    | Named of Namespaced_path.t
    | Unpack

  val empty : t

  val add : Ident.t -> elt -> t -> t

  val singleton : Ident.t -> elt -> t

  val iter : (name:string -> stamp:int -> elt -> unit) -> t -> unit

  val get : Namespaced_path.Id.t -> t -> elt list

  val find_some :
    (string -> int -> elt -> bool) -> t -> (string * int * elt) option
end = struct
  [@@@ocaml.warning "-30"]

  type t = elt StampMap.t String.Map.t

  and elt =
    { loc : Location.t
    ; doc : string option
    ; namespace : Namespaced_path.Namespace.t
    ; node : node }

  (* This sort of merges [Types.module_type] and [Types.signature_item].
     [Mty_ident] and [Mty_alias] are merged (into [Alias])) because from the
     point of view of locate there's no difference between them.
     [Included] is used to remember the origin of things
     Most of the constructors of [signature_item] are represented by a [Leaf]
     node. *)
  and node =
    | Leaf
    | Internal of t Lazy.t
    | Included of include_
    | Alias    of Namespaced_path.t
    | Functor  of functor_parameter * node
    | Apply    of functor_application

  and include_ = (* simply a subset of node. *)
    | Named of Namespaced_path.t
    | Items of t Lazy.t
    | Apply of functor_application

  and functor_parameter =
    (Ident.t option * Location.t * node) option

  and functor_application =
    { funct : Location.t * functor_
    ; arg   : Location.t * node }

  and functor_ =
    | Apply of functor_application
    | Funct of functor_parameter * node
    | Named of Namespaced_path.t
    | Unpack

  let empty = String.Map.empty

  let add id elt t =
    let key = Ident.name id in
    match String.Map.find key t with
    | exception Not_found ->
      String.Map.add ~key ~data:(StampMap.singleton (Ident.stamp id) elt) t
    | stamp_map ->
      (* no replace? :'( *)
      String.Map.add (String.Map.remove key t) ~key
        ~data:(StampMap.add (Ident.stamp id) elt stamp_map)

  let singleton id node = add id node empty

  let iter f t =
    String.Map.iter t ~f:(fun ~key:name ~data ->
      StampMap.iter (fun stamp elt ->
        f ~name ~stamp elt
      ) data
    )

  let get (k : Namespaced_path.Id.t) t =
    match k with
    | Id id ->
      [ StampMap.find (Ident.stamp id)
          (String.Map.find (Ident.name id) t) ]
    | String s ->
      List.map (StampMap.bindings (String.Map.find s t)) ~f:snd

  exception Found of string * int * elt

  let find f (t : t) =
    try
      iter (fun ~name ~stamp data ->
        if f name stamp data then
          raise (Found (name, stamp, data))
      ) t;
      raise Not_found
    with
    | Found (name, stamp, data) -> name, stamp, data

  let find_some f t =
    try Some (find f t)
    with Not_found -> None
end

type t = Trie.t

let extract_doc (attrs : Parsetree.attributes) =
  String.concat ~sep:"\n" (
    List.filter_map attrs ~f:(fun attr ->
      Option.map ~f:fst (Type_utils.read_doc_attributes [attr])
    )
  )


let remove_top_indir =
  List.concat_map ~f:(fun bt ->
    match bt.t_node with
    | Signature _
    | Structure _ -> Lazy.force bt.t_children
    | _ -> [ bt ]
  )

let of_structure s =
  let env, node = Mbrowse.leaf_node (Mbrowse.of_structure s) in
  Browse_tree.of_node ~env node

let of_signature s =
  let env, node = Mbrowse.leaf_node (Mbrowse.of_signature s) in
  Browse_tree.of_node ~env node

let remove_indir_me me =
  match me.Typedtree.mod_desc with
  | Typedtree.Tmod_ident (path, _) -> `Alias path
  | Typedtree.Tmod_structure str -> `Str str
  | Typedtree.Tmod_functor _ ->
    let (fp,me) = Typedtree.unpack_functor_me me in
    `Functor (fp, `Mod_expr me)
  | Typedtree.Tmod_apply (me1, me2, _) -> `Apply (me1, me2)
  | Typedtree.Tmod_constraint (me, _, _, _) -> `Mod_expr me
  | Typedtree.Tmod_unpack _ -> `Unpack

let remove_indir_mty mty =
  match mty.Typedtree.mty_desc with
  | Typedtree.Tmty_alias (path, _) -> `Alias path
  | Typedtree.Tmty_ident (path, _) -> `Ident path
  | Typedtree.Tmty_signature sg -> `Sg sg
  | Typedtree.Tmty_functor _ ->
    let (fp,mty) = Typedtree.unpack_functor_mty mty in
    `Functor (fp, `Mod_type mty)
  | Typedtree.Tmty_with (mty, _) -> `Mod_type mty
  | Typedtree.Tmty_typeof me -> `Mod_expr me

let sig_item_idns item =
  let open Types in
  let ns =
    match item with
    | Sig_value _ -> `Vals
    | Sig_type _ -> `Type
    | Sig_typext _ -> `Type
    | Sig_module _ -> `Mod
    | Sig_modtype _ -> `Modtype
    | Sig_class _ -> `Vals (* that's just silly *)
    | Sig_class_type _ -> `Type (* :_D *)
  in
  signature_item_id item, ns

let include_idents l = List.map ~f:sig_item_idns l

let identify_str_includes item =
  match item.Typedtree.str_desc with
  | Typedtree.Tstr_include { Typedtree. incl_type ; incl_mod ; _ } ->
    `Included (include_idents incl_type, `Mod_expr incl_mod)
  | _ -> `Not_included

let identify_sig_includes item =
  match item.Typedtree.sig_desc with
  | Typedtree.Tsig_include { Typedtree. incl_type ; incl_mod ; _ } ->
    `Included (include_idents incl_type, `Mod_type incl_mod)
  | _ -> `Not_included

let rec build ~local_buffer ~trie browses : t =
  let rec node_for_direct_mod namespace : _ -> Trie.node = function
    | `Alias path -> Alias (Namespaced_path.of_path ~namespace path)
    | `Ident path -> Alias (Namespaced_path.of_path ~namespace:`Modtype path)
    | `Str s ->
      Internal (lazy (build ~local_buffer ~trie:Trie.empty [of_structure s]))
    | `Sg s ->
      Internal (lazy (build ~local_buffer ~trie:Trie.empty [of_signature s]))
    | `Mod_expr me -> node_for_direct_mod `Mod (remove_indir_me me)
    | `Mod_type mty -> node_for_direct_mod `Modtype (remove_indir_mty mty)
    | `Functor (fp, packed) ->
      let param = match fp with
        | Typedtree.Unit -> None
        | Typedtree.Named (id, loc, mty) ->
          let mty =
            if local_buffer
            then node_for_direct_mod `Modtype (remove_indir_mty mty)
            else Trie.Leaf
          in
          Some (id, loc.Location.loc, mty)
      in
      Functor (param, node_for_direct_mod `Mod packed)
    | `Apply (funct, arg) ->
      let funct = funct.Typedtree.mod_loc, functor_ (remove_indir_me funct) in
      let arg =
        arg.Typedtree.mod_loc,
        match remove_indir_me arg with
        | `Str { str_items = []; _ } -> Trie.Leaf
        | otherwise -> node_for_direct_mod `Mod otherwise
      in
      Apply { funct; arg }
    | `Unpack -> (* TODO! *)
      Leaf
  and functor_ : _ -> Trie.functor_ = function
    | `Alias path
    | `Ident path -> Named (Namespaced_path.of_path ~namespace:`Mod path)
    | `Str _
    | `Sg _ -> assert false
    | `Mod_expr me -> functor_ (remove_indir_me me)
    | `Mod_type _ -> assert false
    | `Functor (fp, packed) ->
      let param = match fp with
        | Typedtree.Unit -> None
        | Typedtree.Named (id, loc, mty) ->
          let mty =
            if local_buffer
            then node_for_direct_mod `Modtype (remove_indir_mty mty)
            else Trie.Leaf
          in
          Some (id, loc.Location.loc, mty)
      in
      Funct (param, node_for_direct_mod `Mod packed)
    | `Apply (funct, arg) ->
      let funct = funct.Typedtree.mod_loc, functor_ (remove_indir_me funct) in
      let arg =
        arg.Typedtree.mod_loc, node_for_direct_mod `Mod (remove_indir_me arg)
      in
      Apply { funct; arg }
    | `Unpack -> Unpack
  in
  List.fold_left (remove_top_indir browses) ~init:trie ~f:(fun trie t ->
    let open Typedtree in
    let doc =
      let attrs = node_attributes t.t_node in
      let doc = extract_doc attrs in
      if doc = "" then None else Some doc
    in
    match t.t_node with
    | Signature _
    | Structure _ ->
      (* Removed by [get_top_items] *)
      assert false
    | Signature_item _
    | Structure_item _ ->
      begin match
        match t.t_node with
        | Signature_item (item, _) -> identify_sig_includes item
        | Structure_item (item, _) -> identify_str_includes item
        | _ -> assert false
      with
      | `Not_included -> build ~local_buffer ~trie (Lazy.force t.t_children)
      | `Included (included_idents, packed) ->
        let rec helper packed =
          let f node =
            List.fold_left included_idents ~init:trie ~f:(fun trie (id, ns) ->
              Trie.add id
                { loc = t.t_loc; doc = None; namespace = ns; node } trie
            )
          in
          match
            match packed with
            | `Mod_expr me -> remove_indir_me  me
            | `Mod_type mt -> remove_indir_mty mt
          with
          | `Alias path ->
            let namespace =
              match packed with
              | `Mod_expr _ -> `Mod
              | `Mod_type _ -> `Modtype
            in
            let p = Namespaced_path.of_path ~namespace path in
            f (Included (Named p))
          | `Ident p ->
            let p = Namespaced_path.of_path ~namespace:`Modtype p in
            f (Included (Named p))
          | `Mod_type _
          | `Mod_expr _ as packed -> helper packed
          | `Functor _ ->
            (* You can't include a functor, you can only include "structures". *)
            assert false
          | `Unpack -> f Leaf
          | `Apply (funct, arg) ->
            let funct =
              funct.Typedtree.mod_loc, functor_ (remove_indir_me funct)
            in
            let arg =
              arg.Typedtree.mod_loc,
              node_for_direct_mod `Mod (remove_indir_me arg)
            in
            f (Included (Apply { funct; arg }))
          | `Str str ->
            let str = lazy (build ~local_buffer ~trie [of_structure str]) in
            f (Included (Items str))
          | `Sg  sg ->
            let sg = lazy (build ~local_buffer ~trie [of_signature sg]) in
            f (Included (Items sg))
        in
        helper packed
      end
    | Value_binding vb ->
      let trie =
        List.fold_left ~init:trie ~f:(fun trie (id, { Asttypes.loc; _ }) ->
            Trie.add id {loc; doc; namespace = `Vals; node = Leaf} trie
        ) (Typedtree.pat_bound_idents_with_loc vb.vb_pat)
      in
      if not local_buffer then
        trie
      else (
        let id = Ident.create_local "?" in
        let intern =
          lazy (build ~local_buffer ~trie:Trie.empty (Lazy.force t.t_children))
        in
        let extra_children : Trie.elt =
          { loc = t.t_loc
          ; doc = None
          ; namespace = `Unknown
          ; node = Internal intern }
        in
        Trie.add id extra_children trie
      )
    | Value_description vd ->
      Trie.add vd.val_id
        { loc = t.t_loc; doc; namespace = `Vals; node = Leaf } trie
    | Module_binding mb ->
      let node =
        node_for_direct_mod `Mod
          (remove_indir_me mb.mb_expr)
      in
      begin match Raw_compat.mb_id mb with
        | None -> trie
        | Some id ->
          Trie.add id { loc=t.t_loc; doc; namespace=`Mod; node } trie
      end
    | Module_declaration md ->
      let node =
        node_for_direct_mod `Mod
          (remove_indir_mty md.md_type)
      in
      begin match Raw_compat.md_id md with
        | None -> trie
        | Some id ->
          Trie.add id { loc=t.t_loc; doc; namespace=`Mod; node } trie
      end
    | Module_type_declaration mtd ->
      let node =
        match mtd.mtd_type with
        | None -> Trie.Leaf
        | Some m -> node_for_direct_mod `Modtype (remove_indir_mty m)
      in
      Trie.add mtd.mtd_id
        { loc=t.t_loc; doc; namespace=`Modtype; node } trie
    | Type_declaration td ->
      (* TODO: add constructors and labels as well.
         Because why the hell not. *)
      Trie.add td.typ_id
        { loc = t.t_loc; doc; namespace = `Type; node = Leaf } trie
    | Type_extension te ->
      List.fold_left ~init:trie ~f:(fun trie ec ->
        Trie.add ec.ext_id
          { loc = t.t_loc; doc; namespace = `Type; node = Leaf } trie
      ) te.tyext_constructors
    | Extension_constructor ec ->
      Trie.add ec.ext_id
        { loc = t.t_loc; doc; namespace = `Type; node = Leaf } trie
    | Case _
    | Expression _ when local_buffer ->
      build ~local_buffer ~trie (Lazy.force t.t_children)
    | Pattern p when local_buffer ->
      List.fold_left ~init:trie ~f:(fun trie (id, { Asttypes.loc; _ }) ->
          Trie.add id {loc; doc; namespace = `Vals; node = Leaf} trie
      ) (Typedtree.pat_bound_idents_with_loc p)
    | ignored_node ->
      log ~title:"build" "ignored node: %t"
        (fun () -> string_of_node ignored_node);
      trie
  )

let of_browses ?(local_buffer=false) browses =
  build ~local_buffer ~trie:Trie.empty browses

type scopes = (t * Lexing.position option) list

type functor_argument =
  | Handled of Namespaced_path.t * scopes
  | Noop (* used for generative functors… and non handled constructions *)

type substitution =
  { old_prefix : Namespaced_path.t
  ; new_prefix : Namespaced_path.t
  ; scopes     : scopes }

(* See mli for documentation. *)
type result =
  | Found of Location.t * string option
  | Resolves_to of Namespaced_path.t * state

and state =
  { substs : substitution list
  ; functor_arguments : functor_argument list
  }

let rec follow ~remember_loc ~state scopes ?before trie path =
  let trie, before, path, scopes, state =
    let rec try_substing_once path = function
      | [] -> None
      | { old_prefix; new_prefix; scopes } as subst :: substs ->
        match Namespaced_path.subst_prefix ~old_prefix ~new_prefix path with
        | Some new_path -> Some (new_path, scopes, substs)
        | None ->
          match try_substing_once path substs with
          | None -> None
          | Some (new_path, scopes, remaining_substs) ->
            Some (new_path, scopes, subst :: remaining_substs)
    in
    (* exponential but terminates: at each new step [substs] will have lost one
       element. *)
    let rec try_substing (path, _, substs as acc) =
      match try_substing_once path substs with
      | None -> acc
      | Some new_acc -> try_substing new_acc
    in
    let init = (path, (trie, before) :: scopes, state.substs) in
    let res = try_substing init in
    if res == init then
      trie, before, path, scopes, state
    else
      match res with
      | path, (trie, before) :: scopes, substs ->
        trie, before, path, scopes, { state with substs }
      | _ -> assert false
  in
  let try_next_scope () =
    match scopes with
    | [] -> Resolves_to (path, state) (* no englobing scope, give up *)
    | (trie, before) :: scopes ->
      follow ~remember_loc ~state scopes ?before trie path
  in
  match Namespaced_path.head_exn path with
  | Applied_to path ->
    (* FIXME: That's wrong. The scope should be the one where the query was
       emitted. Not the point where we finally arrive on the application. *)
    let functor_argument =
      let scopes = (trie, before) :: scopes in
      Handled (path, scopes)
    in
    let state =
      { state with
        functor_arguments = functor_argument :: state.functor_arguments
      }
    in
    let new_path = Namespaced_path.peal_head_exn path in
    log ~title:"applicative path" "%s"
      (Namespaced_path.to_unique_string new_path);
    follow ~remember_loc ~state scopes ?before trie new_path
  | Ident (x, namespace) ->
    try
      let lst = Trie.get x trie in
      let lst =
        List.filter lst
          ~f:(fun { Trie.namespace = ns; _ } -> ns = namespace || ns = `Unknown)
      in
      let lst =
        match before with
        | None -> lst
        | Some before ->
          List.filter lst ~f:(fun { Trie.loc; _ } ->
            Lexing.compare_pos loc.Location.loc_start before < 0)
      in
      match
        List.sort lst ~cmp:(fun { Trie.loc = l1; _ } { loc = l2; _ } ->
          (* We wants the ones closed last to be at the beginning of the list. *)
          Lexing.compare_pos l2.Location.loc_end l1.Location.loc_end)
      with
      | [] -> try_next_scope ()
      | { loc; doc; node; namespace = _ } :: _ ->
        let inspect_functor_arg : Trie.node -> _ = function
          | Leaf -> Noop (* fuck it eh. *)
          | Internal (lazy trie) ->
            let scopes =
              (trie, None) :: (trie, Some loc.Location.loc_start) :: scopes
            in
            Handled (Namespaced_path.empty, scopes)
          | Included _ -> assert false (* that surely can't happen *)
          | Alias path ->
            let scopes = (trie, Some loc.Location.loc_start) :: scopes in
            Handled (path, scopes)
          | Functor _ ->
            (* TODO *)
            log ~title:"inspect_functor_arg"
              "NOT HANDLED: functor given as functor argument";
            Noop
          | Apply _ ->
            (* TODO *)
            Noop
        in
        let rec inspect_functor path state : Trie.functor_ -> _ = function
          | Named new_prefix ->
            let path = Namespaced_path.rewrite_head ~new_prefix path in
            log ~title:"inspect_functor" "resolves to %s"
              (Namespaced_path.to_unique_string path);
            follow ~remember_loc ~state ~before:loc.Location.loc_start scopes
              trie path
          | Unpack ->
            log ~title:"inspect_functor" "Unpack";
            Found (loc, doc)
          | Funct _ -> assert false (* TODO *)
          | Apply { funct; arg } ->
            log ~title:"inspect_functor" "functor application";
            let functor_argument = inspect_functor_arg (snd arg) in
            let state =
              { state with
                functor_arguments = functor_argument :: state.functor_arguments
              }
            in
            inspect_functor path state (snd funct)
        in
        let rec inspect_node state = function
          | Trie.Leaf ->
            (* we're not checking whether [xs = []] here, as we wouldn't be able to
              lookup anything else which would be correct I think.
              [xs] can be non-nil in this case when [x] is a first class module.
              ... and perhaps in other situations I am not aware of.  *)
            Found (loc, doc)
          | Alias new_prefix ->
            log ~title:"aliased" "%s%s= %s"
              (Namespaced_path.Id.name x)
              (Namespaced_path.Namespace.to_string namespace)
              (Namespaced_path.to_unique_string new_prefix);
            let path = Namespaced_path.peal_head_exn path in
            let new_path = Namespaced_path.rewrite_head ~new_prefix path in
            remember_loc loc;
            follow ~remember_loc ~state ~before:loc.Location.loc_start scopes
              trie new_path
          | Included include_ ->
            remember_loc loc;
            let stampless = Namespaced_path.strip_stamps path in
            begin match include_ with
            | Named new_prefix ->
              let path = Namespaced_path.rewrite_head ~new_prefix stampless in
              log ~title:"include" "resolves to %s"
                (Namespaced_path.to_unique_string path);
              follow ~remember_loc ~state ~before:loc.Location.loc_start scopes
                trie path
            | Items t ->
              follow ~remember_loc ~state scopes (Lazy.force t) stampless
            | Apply { funct; arg } ->
              log ~title:"include" "functor application";
              let functor_argument = inspect_functor_arg (snd arg) in
              let state =
                { state with
                  functor_arguments =
                    functor_argument :: state.functor_arguments }
              in
              inspect_functor stampless state (snd funct)
            end
          | Internal t ->
            let path = Namespaced_path.peal_head_exn path in
            begin match Namespaced_path.head path with
            | None -> Found (loc, doc)
            | Some _ ->
              let scopes = (trie, Some loc.Location.loc_start) :: scopes in
              follow ~remember_loc ~state ?before scopes (Lazy.force t) path
            end
          | Functor (param, node) ->
            log ~title:"node" "functor";
            let path = Namespaced_path.peal_head_exn path in
            begin match Namespaced_path.head path with
            | None -> Found (loc, doc)
            | _ ->
              let state =
                match state.functor_arguments with
                | [] ->
                  (* We can never end up inside a functor without having seen an
                    application first. *)
                  assert false
                | Noop :: functor_arguments ->
                  { state with functor_arguments }
                | Handled (new_prefix, scopes) :: functor_arguments ->
                  let id = match param with
                    | None | Some (None, _, _) -> assert false (* sigh. *)
                    | Some (Some id, _, _) -> id
                  in
                  let subst =
                    { old_prefix =
                        Namespaced_path.of_path ~namespace:`Mod (Pident id)
                    ; new_prefix
                    ; scopes }
                  in
                  { substs = subst :: state.substs; functor_arguments }
              in
              inspect_node state node
            end
          | Apply { funct; arg } ->
            log ~title:"functor application" "";
            let functor_argument = inspect_functor_arg (snd arg) in
            let state =
              { state with
                functor_arguments = functor_argument :: state.functor_arguments
              }
            in
            inspect_functor (Namespaced_path.peal_head_exn path) state
              (snd funct)
        in
        inspect_node state node
    with
    | Not_found ->
      try_next_scope ()

let initial_state = { substs = []; functor_arguments = [] }

let rec find ~remember_loc ~before scopes trie path =
  match
    Trie.find_some (fun _name _stmap { loc; _ } ->
      Lexing.compare_pos loc.Location.loc_start before < 0
      && Lexing.compare_pos loc.Location.loc_end before > 0
    ) trie
  with
  | None ->
    log ~title:"find" "didn't find anything";
    follow ~state:initial_state ~remember_loc ~before scopes trie path
  | Some (_name, _stamp, { Trie.loc; node; _ }) ->
    log ~title:"find" "inspecting %s" _name;
    let rec inspect_node scopes : Trie.node -> _ = function
      | Internal (lazy subtrie)  ->
        let scopes = (trie, Some loc.Location.loc_start) :: scopes in
        find ~remember_loc ~before scopes subtrie path
      | Functor (None, fnode) -> inspect_node scopes fnode
      | Functor (Some (id, ploc, node), fnode) ->
        let param =
          match id with
          | None -> Trie.empty
          | Some id ->
            Trie.singleton id { loc = ploc; doc = None; namespace = `Mod; node }
        in
        if
          Lexing.compare_pos ploc.Location.loc_start before < 0
          && Lexing.compare_pos ploc.Location.loc_end before > 0
        then
          let scopes = (trie, Some loc.Location.loc_start) :: scopes in
          find ~remember_loc ~before scopes param path
        else
          let scopes = (param, Some ploc.Location.loc_end) :: scopes in
          inspect_node scopes fnode
      | Apply { funct = (floc, f); arg = (_aloc, a) } ->
        let scopes = (trie, Some loc.Location.loc_start) :: scopes in
        if
          Lexing.compare_pos floc.Location.loc_start before < 0
          && Lexing.compare_pos floc.Location.loc_end before > 0
        then
          inspect_functor scopes f
        else
          inspect_node scopes a
      | Leaf
      | Included _
      | Alias _ ->
        (* FIXME: not quite right (i.e. not necessarily a leaf). *)
        log ~title:"find" "cursor in a leaf, so we look only before the leaf";
        follow ~state:initial_state ~remember_loc ~before:loc.Location.loc_start
          scopes trie path
    and inspect_functor scopes : Trie.functor_ -> _ = function
      | Funct (None, fnode) -> inspect_node scopes fnode
      | Funct (Some (id, ploc, node), fnode) ->
        let param =
          match id with
          | None -> Trie.empty
          | Some id ->
            Trie.singleton id { loc = ploc; doc = None; namespace = `Mod; node }
        in
        if
          Lexing.compare_pos ploc.Location.loc_start before < 0
          && Lexing.compare_pos ploc.Location.loc_end before > 0
        then
          let scopes = (trie, Some loc.Location.loc_start) :: scopes in
          find ~remember_loc ~before scopes param path
        else
          let scopes = (param, Some ploc.Location.loc_end) :: scopes in
          inspect_node scopes fnode
      | Apply { funct = (floc, f); arg = (_aloc, a) } ->
        let scopes = (trie, Some loc.Location.loc_start) :: scopes in
        if
          Lexing.compare_pos floc.Location.loc_start before < 0
          && Lexing.compare_pos floc.Location.loc_end before > 0
        then
          inspect_functor scopes f
        else
          inspect_node scopes a
      | Unpack
      | Named _ ->
        (* FIXME: not quite right (i.e. not necessarily a leaf). *)
        log ~title:"find" "cursor in a leaf, so we look only before the leaf";
        follow ~state:initial_state ~remember_loc ~before:loc.Location.loc_start
          scopes trie path
    in
    inspect_node scopes node

type context =
  | Initial of Lexing.position
  | Resume of state

let rec dump fmt trie =
  let open Trie in
  let rec dump_node fmt = function
    | Leaf -> ()
    | Included Named path ->
      Format.fprintf fmt " <%s>" (Namespaced_path.to_string path)
    | Alias path ->
      Format.fprintf fmt " = %s" (Namespaced_path.to_string path)
    | Included Items t
    | Internal t ->
      if Lazy.is_val t then
        Format.fprintf fmt " = %a" dump (Lazy.force t)
      else
        Format.fprintf fmt " = <lazy>"
    | Functor ((None | Some (None, _, _)), node) ->
      Format.fprintf fmt " () ->%a" dump_node node
    | Functor (Some (Some id, _, _), node) ->
      Format.fprintf fmt " %s ->%a" (Ident.name id)
        dump_node node
    | Included Apply { funct; arg }
    | Apply { funct; arg } ->
      Format.fprintf fmt " %a(%a)" dump_functor funct dump_node (snd arg)
  and dump_functor fmt (_loc, funct) =
    match funct with
    | Apply { funct; arg } ->
      Format.fprintf fmt " %a(%a)" dump_functor funct dump_node (snd arg)
    | Funct ((None | Some (None, _, _)), node) ->
      Format.fprintf fmt " () ->%a" dump_node node
    | Funct (Some (Some id, _, _), node) ->
      Format.fprintf fmt " %s ->%a" (Ident.name id) dump_node node
    | Named path ->
      Format.fprintf fmt " = %s" (Namespaced_path.to_string path)
    | Unpack ->
      Format.fprintf fmt " !unpack!"
  in
  let dump_elt {loc; namespace; node; _} =
    Format.pp_print_string fmt (Namespaced_path.Namespace.to_string namespace);
    Location.print_loc fmt loc;
    dump_node fmt node
  in
  Format.pp_print_string fmt "{\n" ;
  Trie.iter (fun ~name ~stamp elt ->
    Format.fprintf fmt "%s/%d -> " name stamp ;
    dump_elt elt;
    Format.pp_print_newline fmt ()
  ) trie;
  Format.pp_print_string fmt "}\n"

let find ~remember_loc ~context trie path =
  match context with
  | Initial before ->
    log ~title:"initial find"
      "before %s, trie: %a"
      (Lexing.print_position () before)
      Logger.fmt (fun fmt -> dump fmt trie);
    find ~remember_loc ~before [] trie path
  | Resume   state -> follow ~remember_loc ~state [] trie path
