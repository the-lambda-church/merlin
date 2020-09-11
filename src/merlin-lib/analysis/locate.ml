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

let loadpath     = ref []

let last_location = ref Location.none

let {Logger. log} = Logger.for_section "locate"

let erase_loadpath ~cwd ~new_path k =
  let str_path_list =
    List.map new_path ~f:(function
      | "" ->
        (* That's the cwd at the time of the generation of the cmt, I'm
            guessing/hoping it will be the directory where we found it *)
        log ~title:"erase_loadpath" "%s" cwd;
        cwd
      | x ->
        log ~title:"erase_loadpath" "%s" x;
        x
    )
  in
  let_ref loadpath str_path_list k

let restore_loadpath ~config k =
  log ~title:"restore_loadpath" "Restored load path";
  let_ref loadpath (Mconfig.cmt_path config) k

module Fallback = struct
  let fallback = ref None

  let get () = !fallback

  let set loc =
    log ~title:"Fallback.set"
      "%a" Logger.fmt (fun fmt -> Location.print_loc fmt loc);
    fallback := Some loc

  let reset () = fallback := None

  let is_set () = !fallback <> None
end

module File : sig
  type t = private
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  val ml : string -> t
  val mli : string -> t
  val cmt : string -> t
  val cmti : string -> t

  val of_filename : string -> t option

  val alternate : t -> t

  val name : t -> string

  val with_ext : ?src_suffix_pair:(string * string) -> t -> string

  val explain_not_found :
    ?doc_from:string -> string -> t -> [> `File_not_found of string ]
end = struct
  type t =
    | ML   of string
    | MLL  of string
    | MLI  of string
    | CMT  of string
    | CMTI of string

  let file_path_to_mod_name f =
    Misc.unitname (Filename.basename f)

  let ml   s = ML   (file_path_to_mod_name s)
  let mll  s = MLL  (file_path_to_mod_name s)
  let mli  s = MLI  (file_path_to_mod_name s)
  let cmt  s = CMT  (file_path_to_mod_name s)
  let cmti s = CMTI (file_path_to_mod_name s)

  let of_filename fn =
    match Misc.rev_string_split ~on:'.' fn with
    | []
    | [ _ ] -> None
    | ext :: _ ->
      let ext = String.lowercase ext in
      Some (
        match ext with
        | "cmti" -> cmti fn
        | "cmt"  -> cmt fn
        | "mll"  -> mll fn
        | _ -> if Filename.check_suffix ext "i" then mli fn else ml fn
      )

  let alternate = function
    | ML  s
    | MLL s -> MLI s
    | MLI s -> ML s
    | CMT s  -> CMTI s
    | CMTI s -> CMT s

  let name = function
    | ML name
    | MLL name
    | MLI name
    | CMT name
    | CMTI name -> name

  let ext src_suffix_pair = function
    | ML _  -> fst src_suffix_pair
    | MLI _  -> snd src_suffix_pair
    | MLL _ -> ".mll"
    | CMT _ -> ".cmt"
    | CMTI _ -> ".cmti"

  let with_ext ?(src_suffix_pair=(".ml",".mli")) t =
    name t ^ ext src_suffix_pair t

  let explain_not_found ?(doc_from="") str_ident path =
    let msg =
      match path with
      | ML file ->
        sprintf "'%s' seems to originate from '%s' whose ML file could not be \
                 found" str_ident file
      | MLL file ->
        sprintf "'%s' seems to originate from '%s' whose MLL file could not be \
                 found" str_ident file
      | MLI file ->
        sprintf "'%s' seems to originate from '%s' whose MLI file could not be \
                 found" str_ident file
      | CMT file ->
        sprintf "Needed cmt file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI file when file <> doc_from ->
        sprintf "Needed cmti file of module '%s' to locate '%s' but it is not \
                 present" file str_ident
      | CMTI _ ->
        sprintf "The documentation for '%s' originates in the current file, \
                 but no cmt is available" str_ident
    in
    `File_not_found msg
end

module Preferences : sig
  val set : [ `ML | `MLI ] -> unit

  val src : string -> File.t
  val build : string -> File.t

  val is_preferred : string -> bool
end = struct
  let prioritize_impl = ref true

  let set choice =
    prioritize_impl :=
      match choice with
      | `ML -> true
      | _ -> false

  let src   file = if !prioritize_impl then File.ml  file else File.mli  file
  let build file = if !prioritize_impl then File.cmt file else File.cmti file

  let is_preferred fn =
    match File.of_filename fn with
    | Some ML _ -> !prioritize_impl
    | Some MLI _ -> not !prioritize_impl
    | _ -> false
end

module File_switching : sig
  val reset : unit -> unit

  val move_to : digest:Digest.t -> string -> unit

  val where_am_i : unit -> string option

  val source_digest : unit -> Digest.t option
end = struct
  type t = {
    last_file_visited : string;
    digest : Digest.t;
  }

  let last_file_visited t = t.last_file_visited
  let digest t = t.digest

  let state = ref None

  let reset () = state := None

  let move_to ~digest file =
    log ~title:"File_switching.move_to" "%s" file;
    state := Some { last_file_visited = file ; digest }

  let where_am_i () = Option.map !state ~f:last_file_visited

  let source_digest () = Option.map !state ~f:digest
end


module Utils = struct
  let is_builtin_path = function
    | Path.Pident id ->
      let f (_, i) = Ident.same i id in
      List.exists Predef.builtin_idents ~f
      || List.exists Predef.builtin_values ~f
    | _ -> false

  let is_ghost_loc { Location. loc_ghost; _ } = loc_ghost

  (* Reuse the code of [Misc.find_in_path_uncap] but returns all the files
     matching, instead of the first one.
     This is only used when looking for ml files, not cmts. Indeed for cmts we
     know that the load path will only ever contain files with uniq names (in
     the presence of packed modules we refine the loadpath as we go); this in
     not the case for the "source path" however.
     We therefore get all matching files and use an heuristic at the call site
     to choose the appropriate file. *)
  let find_all_in_path_uncap ?src_suffix_pair ~with_fallback path file =
    let name = File.with_ext ?src_suffix_pair file in
    let uname = String.uncapitalize name in
    let fallback, ufallback =
      let alt = File.alternate file in
      let fallback = File.with_ext ?src_suffix_pair alt in
      fallback, String.uncapitalize fallback
    in
    let try_file dirname basename acc =
      if Misc.exact_file_exists ~dirname ~basename
      then Misc.canonicalize_filename (Filename.concat dirname basename) :: acc
      else acc
    in
    let try_dir acc dirname =
      let acc = try_file dirname uname acc in
      let acc = try_file dirname name acc in
      let acc =
        if with_fallback then
          let acc = try_file dirname ufallback acc in
          let acc = try_file dirname fallback acc in
          acc
        else
          acc
      in
      acc
    in
    List.fold_left ~f:try_dir ~init:[] path

  let find_all_matches ~config ?(with_fallback=false) file =
    let files =
      List.concat_map ~f:(fun synonym_pair ->
        find_all_in_path_uncap ~src_suffix_pair:synonym_pair ~with_fallback
          (Mconfig.source_path config) file
      ) Mconfig.(config.merlin.suffixes)
    in
    List.uniq files ~cmp:String.compare

  let find_file_with_path ~config ?(with_fallback=false) file path =
    if File.name file = Misc.unitname Mconfig.(config.query.filename) then
      Some Mconfig.(config.query.filename)
    else
      let attempt_search src_suffix_pair =
        let fallback =
          if with_fallback then
            Some (File.with_ext ~src_suffix_pair (File.alternate file))
          else
            None
        in
        let fname = File.with_ext ~src_suffix_pair file in
        try Some (Misc.find_in_path_uncap ?fallback path fname)
        with Not_found -> None
      in
      try
        Some (List.find_map Mconfig.(config.merlin.suffixes) ~f:attempt_search)
      with Not_found ->
        None

  let find_file ~config ?with_fallback (file : File.t) =
    find_file_with_path ~config ?with_fallback file @@
        match file with
        | ML  _ | MLI _  | MLL _ -> Mconfig.source_path config
        | CMT _ | CMTI _         -> !loadpath
end

exception Cmt_cache_store of Typedtrie.t

let trie_of_cmt root =
  let open Cmt_format in
  let cached = Cmt_cache.read root in
  log ~title:"browse_cmts" "inspecting %s" root ;
  begin match cached.Cmt_cache.location_trie with
  | Cmt_cache_store _ ->
    let digest =
      (* [None] only for packs, and we wouldn't have a trie if the cmt was for a
         pack. *)
      Option.get cached.cmt_infos.cmt_source_digest
    in
    File_switching.move_to ~digest root;
    log ~title:"browse_cmts" "trie already cached"
  | Not_found ->
    let trie_of_nodes nodes =
      let digest =
        (* [None] only for packs. *)
        Option.get cached.cmt_infos.cmt_source_digest
      in
      File_switching.move_to ~digest root;
      let trie =
        Typedtrie.of_browses (List.map ~f:Browse_tree.of_node nodes)
      in
      cached.location_trie <- Cmt_cache_store trie
    in
    Option.iter ~f:trie_of_nodes (
      match cached.Cmt_cache.cmt_infos.cmt_annots with
      | Packed (_, _)       -> None
      | Interface intf      -> Some [Browse_raw.Signature intf]
      | Implementation impl -> Some [Browse_raw.Structure impl]
      | Partial_interface parts
      | Partial_implementation parts ->
        log ~title:"browse_cmt" "working from partial cmt(i)";
        let env = cached.cmt_infos.cmt_initial_env in
        let nodes =
          Array.to_list parts
          |> List.map ~f:(Mbrowse.node_of_binary_part env)
        in
        Some nodes
    )
  | _ -> assert false
  end;
  cached.cmt_infos, cached.location_trie

type locate_result =
  | Found of Location.t * string option
  | File_not_found of File.t
  | Other_error (* FIXME *)

let rec locate ~config ~context path trie : locate_result =
  match Typedtrie.find ~remember_loc:Fallback.set ~context trie path with
  | Typedtrie.Found (loc, doc_opt) -> Found (loc, doc_opt)
  | Typedtrie.Resolves_to (new_path, state) ->
    begin match Namespaced_path.head_exn new_path with
    | Ident (_, `Mod) ->
      log ~title:"locate" "resolves to %s" (Namespaced_path.to_unique_string new_path);
      from_path ~config ~context:(Typedtrie.Resume state) new_path
    | _ ->
      log ~title:"locate" "new path (%s) is not a real path"
        (Namespaced_path.to_unique_string new_path);
      log ~title:"locate (typedtrie dump)" "%a"
        Logger.fmt (fun fmt -> Typedtrie.dump fmt trie);
      Other_error (* incorrect path *)
    end

and from_path ~config ~context path : locate_result =
  log ~title:"from_path" "%s" (Namespaced_path.to_unique_string path) ;
  match Namespaced_path.head_exn path with
  | Ident (fname, `Mod) ->
    let path = Namespaced_path.peal_head_exn path in
    let fname = Namespaced_path.Id.name fname in
    let file = Preferences.build fname in
    let browse_cmt cmt_file =
      let cmt_infos, trie = trie_of_cmt cmt_file in
      match trie, Namespaced_path.head path with
      | Not_found, None ->
        Other_error (* Trying to stop on a packed module... *)
      | Not_found, Some _ ->
        log ~title:"from_path" "Saw packed module => erasing loadpath" ;
        erase_loadpath ~cwd:(Filename.dirname cmt_file)
          ~new_path:cmt_infos.cmt_loadpath
          (fun () -> from_path ~context ~config path)
      | Cmt_cache_store _, None ->
        (* We found the module we were looking for, we can stop here. *)
        let pos_fname =
          match cmt_infos.cmt_sourcefile with
          | None   -> fname
          | Some f -> f
        in
        let pos = Lexing.make_pos ~pos_fname (1, 0) in
        let loc = { Location. loc_start=pos ; loc_end=pos ; loc_ghost=true } in
        (* TODO: retrieve "ocaml.text" floating attributes? *)
        Found (loc, None)
      | Cmt_cache_store trie, Some _ ->
        locate ~config ~context path trie
      | _, _ -> assert false
    in
    begin match Utils.find_file ~config ~with_fallback:true file with
    | Some cmt_file -> browse_cmt cmt_file
    | None ->
      (* The following is ugly, and deserves some explanations:
           As can be seen above, when encountering packed modules we override
           the loadpath by the one used to create the pack.
           This means that if the cmt files haven't been moved, we have access
           to the cmt file of every unit included in the pack.
           However, we might not have access to any other cmt (e.g. if others
           paths in the loadpath reference only cmis of packs).
           (Note that if we had access to other cmts, there might be conflicts,
           and the paths order would matter unless we have reliable digests...)
           Assuming we are in such a situation, if we do not find something in
           our "erased" loadpath, it could mean that we are looking for a
           persistent unit, and that's why we restore the initial loadpath. *)
      restore_loadpath ~config (fun () ->
        match Utils.find_file ~config ~with_fallback:true file with
        | Some cmt_file -> browse_cmt cmt_file
        | None ->
          log ~title:"from_path" "failed to locate the cmt[i] of '%s'" fname;
          File_not_found file
      )
    end
  | _ ->
    Other_error (* type error, [from_path] should only be called on modules *)

let path_and_loc_of_cstr desc _ =
  let open Types in
  match desc.cstr_tag with
  | Cstr_extension (path, _) -> path, desc.cstr_loc
  | _ ->
    match desc.cstr_res.desc with
    | Tconstr (path, _, _) -> path, desc.cstr_loc
    | _ -> assert false

let path_and_loc_from_label desc env =
  let open Types in
  match desc.lbl_res.desc with
  | Tconstr (path, _, _) ->
    let typ_decl = Env.find_type path env in
    path, typ_decl.Types.type_loc
  | _ -> assert false

type find_source_result =
  | Found of string
  | Not_found of File.t
  | Multiple_matches of string list

let find_source ~config loc =
  let fname = loc.Location.loc_start.Lexing.pos_fname in
  let with_fallback = loc.Location.loc_ghost in
  let file =
    match File.of_filename fname with
    | Some file -> file
    | None ->
      (* no extension? we have to decide. *)
      Preferences.src fname
  in
  let filename = File.name file in
  let initial_path =
    match File_switching.where_am_i () with
    | None -> fname
    | Some s -> s
  in
  let dir = Filename.dirname initial_path in
  let dir =
    match Mconfig.(config.query.directory) with
    | "" -> dir
    | cwd -> Misc.canonicalize_filename ~cwd dir
  in
  match Utils.find_all_matches ~config ~with_fallback file with
  | [] ->
    log ~title:"find_source" "failed to find %S in source path (fallback = %b)"
       filename with_fallback ;
    log ~title:"find_source" "looking for %S in %S" (File.name file) dir ;
    begin match Utils.find_file_with_path ~config ~with_fallback file [dir] with
    | Some source -> Found source
    | None ->
      log ~title:"find_source" "Trying to find %S in %S directly" fname dir;
      try Found (Misc.find_in_path [dir] fname)
      with _ -> Not_found file
    end
  | [ x ] -> Found x
  | files ->
    log ~title:(sprintf "find_source(%s)" filename)
      "multiple matches in the source path : %s"
      (String.concat ~sep:" , " files);
    try
      match File_switching.source_digest () with
      | None ->
        log ~title:"find_source"
          "... no source digest available to select the right one" ;
        raise Not_found
      | Some digest ->
        log ~title:"find_source"
          "... trying to use source digest to find the right one" ;
        log ~title:"find_source" "Source digest: %s" (Digest.to_hex digest) ;
        Found (
          List.find files ~f:(fun f ->
            let fdigest = Digest.file f in
            log ~title:"find_source" "  %s (%s)" f (Digest.to_hex fdigest) ;
            fdigest = digest
          )
        )
    with Not_found ->
      log ~title:"find_source" "... using heuristic to select the right one" ;
      log ~title:"find_source" "we are looking for a file named %s in %s" fname dir ;
      let rev = String.reverse (Misc.canonicalize_filename ~cwd:dir fname) in
      let lst =
        List.map files ~f:(fun path ->
          let path' = String.reverse path in
          let priority = (String.common_prefix_len rev path') * 2 +
                          if Preferences.is_preferred path
                          then 1
                          else 0
          in
          priority, path
        )
      in
      let lst =
        (* TODO: remove duplicates in [source_path] instead of using
          [sort_uniq] here. *)
        List.sort_uniq ~cmp:(fun ((i:int),s) ((j:int),t) ->
          let tmp = compare j i in
          if tmp <> 0 then tmp else
          match compare s t with
          | 0 -> 0
          | n ->
            (* Check if we are referring to the same files.
                Especially useful on OSX case-insensitive FS.
                FIXME: May be able handle symlinks and non-existing files,
                CHECK *)
            match File_id.get s, File_id.get t with
            | s', t' when File_id.check s' t' ->
              0
            | _ -> n
        ) lst
      in
      match lst with
      | (i1, _) :: (i2, _) :: _ when i1 = i2 ->
        Multiple_matches files
      | (_, s) :: _ -> Found s
      | _ -> assert false

(* Well, that's just another hack.
   [find_source] doesn't like the "-o" option of the compiler. This hack handles
   Jane Street specific use case where "-o" is used to prefix a unit name by the
   name of the library which contains it. *)
let find_source ~config loc path =
  let result =
    match find_source ~config loc with
    | Found _ as result -> result
    | failure ->
      let fname = loc.Location.loc_start.Lexing.pos_fname in
      match
        let i = String.first_double_underscore_end fname in
        let pos = i + 1 in
        let fname = String.sub fname ~pos ~len:(String.length fname - pos) in
        let loc =
          let lstart = { loc.Location.loc_start with Lexing.pos_fname = fname } in
          { loc with Location.loc_start = lstart }
        in
        find_source ~config loc
      with
      | Found _ as result -> result
      | _ -> failure
      | exception _ -> failure
  in
  match result with
  | Found src -> `Found (Some src, loc.Location.loc_start)
  | Not_found f -> File.explain_not_found path f
  | Multiple_matches lst ->
    let matches = String.concat lst ~sep:", " in
    `File_not_found (
      sprintf "Several source files in your path have the same name, and \
               merlin doesn't know which is the right one: %s"
        matches)

let recover _ =
  match Fallback.get () with
  | None -> assert false
  | Some loc -> `Found (loc, None)

module Namespace = struct
  type under_type = [ `Constr | `Labels ]

  type t = (* TODO: share with [Namespaced_path.Namespace.t] *)
    [ `Type | `Mod | `Modtype | `Vals | under_type ]

  type inferred =
    [ t
    | `This_label of Types.label_description
    | `This_cstr of Types.constructor_description ]

  let from_context : Context.t -> inferred list = function
    | Type          -> [ `Type ; `Mod ; `Modtype ; `Constr ; `Labels ; `Vals ]
    | Module_type   -> [ `Modtype ; `Mod ; `Type ; `Constr ; `Labels ; `Vals ]
    | Expr          -> [ `Vals ; `Mod ; `Modtype ; `Constr ; `Labels ; `Type ]
    | Patt          -> [ `Mod ; `Modtype ; `Type ; `Constr ; `Labels ; `Vals ]
    | Unknown       -> [ `Vals ; `Type ; `Constr ; `Mod ; `Modtype ; `Labels ]
    | Label lbl     -> [ `This_label lbl ]
    | Constructor c -> [ `This_cstr c ]
    | Module_path   -> [ `Mod ]
end

module Env_lookup : sig

  val loc
    : Path.t
    -> Namespaced_path.Namespace.t
    -> Env.t
    -> Location.t option

  val in_namespaces
     : Namespace.inferred list
    -> Longident.t
    -> Env.t
    -> (Path.t * Namespaced_path.t * Location.t) option

end = struct

  let loc path (namespace : Namespaced_path.Namespace.t) env =
    try
      Some (
        match namespace with
        | `Unknown
        | `Apply
        | `Vals -> (Env.find_value path env).val_loc
        | `Constr
        | `Labels
        | `Type -> (Env.find_type path env).type_loc
        | `Functor
        | `Mod -> (Env.find_module path env).md_loc
        | `Modtype -> (Env.find_modtype path env).mtd_loc)
    with
      Not_found -> None

  exception Found of (Path.t * Namespaced_path.t * Location.t)

  let in_namespaces (nss : Namespace.inferred list) ident env =
    try
      List.iter nss ~f:(fun namespace ->
        try
          match namespace with
          | `This_cstr cd ->
            log ~title:"lookup"
              "got constructor, fetching path and loc in type namespace";
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
          | `Constr ->
            log ~title:"lookup" "lookup in constructor namespace" ;
            let cd = Env.find_constructor_by_name ident env in
            let path, loc = path_and_loc_of_cstr cd env in
            (* TODO: Use [`Constr] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
          | `Mod ->
            log ~title:"lookup" "lookup in module namespace" ;
            let path, md = Env.find_module_by_name ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Mod path, md.Types.md_loc))
          | `Modtype ->
            log ~title:"lookup" "lookup in module type namespace" ;
            let path, mtd = Env.find_modtype_by_name ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Modtype path, mtd.Types.mtd_loc))
          | `Type ->
            log ~title:"lookup" "lookup in type namespace" ;
            let path, typ_decl = Env.find_type_by_name ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, typ_decl.Types.type_loc))
          | `Vals ->
            log ~title:"lookup" "lookup in value namespace" ;
            let path, val_desc = Env.find_value_by_name ident env in
            raise (Found (path, Namespaced_path.of_path ~namespace:`Vals path, val_desc.Types.val_loc))
          | `This_label lbl ->
            log ~title:"lookup"
              "got label, fetching path and loc in type namespace";
            let path, loc = path_and_loc_from_label lbl env in
            (* TODO: Use [`Labels] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
          | `Labels ->
            log ~title:"lookup" "lookup in label namespace" ;
            let lbl = Env.find_label_by_name ident env in
            let path, loc = path_and_loc_from_label lbl env in
            (* TODO: Use [`Labels] here instead of [`Type] *)
            raise (Found (path, Namespaced_path.of_path ~namespace:`Type path, loc))
        with Not_found -> ()
      ) ;
      log ~title:"lookup" "   ... not in the environment" ;
      None
    with Found x ->
      Some x
end

let locate ~config ~ml_or_mli ~path ~lazy_trie ~pos ~str_ident loc =
  File_switching.reset ();
  Fallback.reset ();
  Preferences.set ml_or_mli;
  log ~title:"locate"
    "present in the environment, walking up the typedtree looking for '%s'"
    (Namespaced_path.to_unique_string path);
  try
    if not (Utils.is_ghost_loc loc) then Fallback.set loc;
    let lazy trie = lazy_trie in
    match locate ~config ~context:(Initial pos) path trie with
    | Found (loc, doc) -> `Found (loc, doc)
    | Other_error
    | File_not_found _ when Fallback.is_set () -> recover str_ident
    | Other_error -> `Not_found (str_ident, File_switching.where_am_i ())
    | File_not_found f -> File.explain_not_found str_ident f
  with
  | _ when Fallback.is_set () -> recover str_ident
  | Not_found -> `Not_found (str_ident, File_switching.where_am_i ())

(* Only used to retrieve documentation *)
let from_completion_entry ~config ~lazy_trie ~pos (namespace, path, loc) =
  let str_ident = Path.name path in
  let tagged_path = Namespaced_path.of_path ~namespace path in
  locate ~config ~ml_or_mli:`MLI ~path:tagged_path ~pos ~str_ident loc
    ~lazy_trie

let from_longident ~config ~env ~lazy_trie ~pos nss ml_or_mli ident =
  let str_ident = String.concat ~sep:"." (Longident.flatten ident) in
  match Env_lookup.in_namespaces nss ident env with
  | None -> `Not_in_env str_ident
  | Some (path, tagged_path, loc) ->
    if Utils.is_builtin_path path then
      `Builtin
    else
      locate ~config ~ml_or_mli ~path:tagged_path ~lazy_trie ~pos ~str_ident loc

let from_path ~config ~env ~local_defs ~pos ~namespace ml_or_mli path =
  let str_ident = Path.name path in
  if Utils.is_builtin_path path then
    `Builtin
  else
    let browse = Mbrowse.of_typedtree local_defs in
    let lazy_trie =
      lazy (Typedtrie.of_browses ~local_buffer:true
              [Browse_tree.of_browse browse])
    in
    let nss_path = Namespaced_path.of_path ~namespace path in
    match Env_lookup.loc path namespace env with
    | None -> `Not_in_env str_ident
    | Some loc ->
      match
        locate ~config ~ml_or_mli ~path:nss_path ~lazy_trie ~pos ~str_ident loc
      with
      | `Not_found _
      | `File_not_found _ as err -> err
      | `Found (loc, _) -> find_source ~config loc str_ident

let from_string ~config ~env ~local_defs ~pos ?namespaces switch path =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie =
    lazy (Typedtrie.of_browses ~local_buffer:true
            [Browse_tree.of_browse browse])
  in
  let lid = Longident.parse path in
  let ident, is_label = Longident.keep_suffix lid in
  match
    match namespaces with
    | Some nss ->
      if not is_label
      then `Ok (nss :> Namespace.inferred list)
      else if List.mem `Labels ~set:nss then (
        log ~title:"from_string" "restricting namespaces to labels";
        `Ok [ `Labels ]
      ) else (
        log ~title:"from_string"
          "input is clearly a label, but the given namespaces don't cover that";
        `Error `Missing_labels_namespace
      )
    | None ->
      match Context.inspect_browse_tree ~cursor:pos lid [browse], is_label with
      | None, _ ->
        log ~title:"from_string" "already at origin, doing nothing" ;
        `Error `At_origin
      | Some (Label _ as ctxt), true
      | Some ctxt, false ->
        log ~title:"from_string"
          "inferred context: %s" (Context.to_string ctxt);
        `Ok (Namespace.from_context ctxt)
      | _, true ->
        log ~title:"from_string"
          "dropping inferred context, it is not precise enough";
        `Ok [ `Labels ]
  with
  | `Error e -> e
  | `Ok nss ->
    log ~title:"from_string"
      "looking for the source of '%s' (prioritizing %s files)"
      path (match switch with `ML -> ".ml" | `MLI -> ".mli");
    let_ref loadpath (Mconfig.cmt_path config) @@ fun () ->
    match from_longident ~config ~pos ~env ~lazy_trie nss switch ident with
    | `File_not_found _ | `Not_found _ | `Not_in_env _ as err -> err
    | `Builtin -> `Builtin path
    | `Found (loc, _) -> find_source ~config loc path

let get_doc ~config ~env ~local_defs ~comments ~pos =
  let browse = Mbrowse.of_typedtree local_defs in
  let lazy_trie = lazy (Typedtrie.of_browses ~local_buffer:true
                          [Browse_tree.of_browse browse]) in
  fun path ->
  let_ref loadpath (Mconfig.cmt_path config) @@ fun () ->
  let_ref last_location Location.none @@ fun () ->
  match
    match path with
    | `Completion_entry entry -> from_completion_entry ~config ~pos ~lazy_trie entry
    | `User_input path ->
      let lid = Longident.parse path in
      begin match Context.inspect_browse_tree ~cursor:pos lid [browse] with
      | None ->
        `Found ({ Location. loc_start=pos; loc_end=pos ; loc_ghost=true }, None)
      | Some ctxt ->
        let nss = Namespace.from_context ctxt in
        log ~title:"get_doc" "looking for the doc of '%s'" path ;
        from_longident ~config ~pos ~env ~lazy_trie nss `MLI lid
      end
  with
  | `Found (_, Some doc) ->
    `Found doc
  | `Found (loc, None) ->
    let comments =
      match File_switching.where_am_i () with
      | None -> List.rev comments
      | Some cmt_path ->
        let {Cmt_cache. cmt_infos; _ } = Cmt_cache.read cmt_path in
        cmt_infos.Cmt_format.cmt_comments
    in
    log ~title:"get_doc" "%a" Logger.fmt (fun fmt ->
        Format.fprintf fmt "looking around %a inside: [\n"
          Location.print_loc !last_location;
        List.iter comments ~f:(fun (c, l) ->
            Format.fprintf fmt "  (%S, %a);\n" c
              Location.print_loc l);
        Format.fprintf fmt "]\n"
      );
    begin match
      Ocamldoc.associate_comment comments loc !last_location
    with
    | None, _     -> `No_documentation
    | Some doc, _ -> `Found doc
    end
  | `Builtin ->
    begin match path with
    | `User_input path -> `Builtin path
    | `Completion_entry (_, path, _) -> `Builtin (Path.name path)
    end
  | `File_not_found _
  | `Not_found _
  | `Not_in_env _ as otherwise -> otherwise
