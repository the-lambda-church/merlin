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

open Misc
open Std
open Query_protocol
module Printtyp = Type_utils.Printtyp

exception No_nodes

let print_completion_entries ~with_types config source entries =
  if with_types then
    let input_ref = ref [] and output_ref = ref [] in
    let preprocess entry =
      match Completion.raw_info_printer entry with
      | `String s -> `String s
      | `Print t ->
        let r = ref "" in
        input_ref := t :: !input_ref;
        output_ref := r :: !output_ref;
        `Print r
      | `Concat (s,t) ->
        let r = ref "" in
        input_ref := t :: !input_ref;
        output_ref := r :: !output_ref;
        `Concat (s,r)
    in
    let entries = List.rev_map ~f:(Completion.map_entry preprocess) entries in
    let entries = List.rev entries in
    let outcomes = Mreader.print_batch_outcome config source !input_ref in
    List.iter2 ~f:(:=) !output_ref outcomes;
    let postprocess = function
      | `String s -> s
      | `Print r -> !r
      | `Concat (s,r) -> s ^ !r
    in
    List.rev_map ~f:(Completion.map_entry postprocess) entries
  else List.rev_map ~f:(Completion.map_entry (fun _ -> "")) entries

let for_completion pipeline position =
  let pipeline = Mpipeline.for_completion position pipeline in
  let typer = Mpipeline.typer_result pipeline in
  (pipeline, typer)

let verbosity pipeline =
  Mconfig.((Mpipeline.final_config pipeline).query.verbosity)

let dump pipeline = function
  | [`String "ppxed-source"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.ppx_parsetree pipeline with
      | `Interface s -> Pprintast.signature ppf s
      | `Implementation s -> Pprintast.structure ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | [`String "source"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.reader_parsetree pipeline with
      | `Interface s -> Pprintast.signature ppf s
      | `Implementation s -> Pprintast.structure ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | [`String "parsetree"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.reader_parsetree pipeline with
      | `Interface s -> Printast.interface ppf s
      | `Implementation s -> Printast.implementation ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | [`String "ppxed-parsetree"] ->
    let ppf, to_string = Format.to_string () in
    begin match Mpipeline.ppx_parsetree pipeline with
      | `Interface s -> Printast.interface ppf s
      | `Implementation s -> Printast.implementation ppf s
    end;
    Format.pp_print_newline ppf ();
    Format.pp_force_newline ppf ();
    `String (to_string ())

  | (`String ("env" | "fullenv" as kind) :: opt_pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let kind = if kind = "env" then `Normal else `Full in
    let pos =
      match opt_pos with
      |  [`String "at"; jpos] ->
        Some (match jpos with
            | `String "start" -> `Start
            | `String "end" -> `End
            | `Int offset -> `Offset offset
            | `Assoc props ->
              begin match List.assoc "line" props, List.assoc "col" props with
                | `Int line, `Int col -> `Logical (line,col)
                | _ -> failwith "Incorrect position"
                | exception Not_found -> failwith "Incorrect position"
              end
            | _ -> failwith "Incorrect position"
          )
      | [] -> None
      | _ -> failwith "incorrect position"
    in
    let env = match pos with
      | None -> Mtyper.get_env typer
      | Some pos ->
        let pos = Mpipeline.get_lexing_pos pipeline pos in
        fst (Mbrowse.leaf_node (Mtyper.node_at typer pos))
    in
    let sg = Browse_misc.signature_of_env ~ignore_extensions:(kind = `Normal) env in
    let aux item =
      let ppf, to_string = Format.to_string () in
      Printtyp.signature ppf [item];
      `String (to_string ())
    in
    `List (List.map ~f:aux sg)

  | [`String "browse"] ->
    let typer = Mpipeline.typer_result pipeline in
    let structure = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    Browse_misc.dump_browse (snd (Mbrowse.leaf_node structure))

  | [`String "tokens"] ->
    failwith "TODO"

  | [`String "flags"] ->
    let prepare_flags flags =
      Json.list Json.string (List.concat_map flags ~f:(fun f -> f.workval)) in
    let user = prepare_flags
        Mconfig.((Mpipeline.input_config pipeline).merlin.flags_to_apply) in
    let applied = prepare_flags
        Mconfig.((Mpipeline.final_config pipeline).merlin.flags_applied) in
    `Assoc [ "user", user; "applied", applied ]

  | [`String "warnings"] ->
    let _typer = Mpipeline.typer_result pipeline in
    Warnings.dump () (*TODO*)

  | [`String "exn"] ->
    let exns =
      Mpipeline.reader_lexer_errors pipeline @
      Mpipeline.reader_parser_errors pipeline @
      Mpipeline.typer_errors pipeline
    in
    `List (List.map ~f:(fun x -> `String (Printexc.to_string x)) exns)

  | [`String "paths"] ->
    let paths = Mconfig.build_path (Mpipeline.final_config pipeline) in
    `List (List.map paths ~f:(fun s -> `String s))

  | _ -> failwith "known dump commands: \
                   paths, exn, warnings, flags, tokens, browse, source, \
                   parsetree, ppxed-source, ppxed-parsetree, \
                   env/fullenv (at {col:, line:})"

let reconstruct_identifier pipeline pos = function
  | None ->
    let path = Mreader.reconstruct_identifier
        (Mpipeline.input_config pipeline)
        (Mpipeline.raw_source pipeline)
        pos
    in
    let path = Mreader_lexer.identifier_suffix path in
    Logger.log
      ~section:Type_enclosing.log_section
      ~title:"reconstruct-identifier"
      "paths: [%s]"
      (String.concat ~sep:";" (List.map path
        ~f:(fun l -> l.Location.txt)));
    let reify dot =
      if dot = "" ||
         (dot.[0] >= 'a' && dot.[0] <= 'z') ||
         (dot.[0] >= 'A' && dot.[0] <= 'Z')
      then dot
      else "(" ^ dot ^ ")"
    in
    begin match path with
      | [] -> []
      | base :: tail ->
        let f {Location. txt=base; loc=bl} {Location. txt=dot; loc=dl} =
          let loc = Location_aux.union bl dl in
          let txt = base ^ "." ^ reify dot in
          Location.mkloc txt loc
        in
        [ List.fold_left tail ~init:base ~f ]
    end
  | Some (expr, offset) ->
    let loc_start =
      let l, c = Lexing.split_pos pos in
      Lexing.make_pos (l, c - offset)
    in
    let shift loc int =
      let l, c = Lexing.split_pos loc in
      Lexing.make_pos (l, c + int)
    in
    let add_loc source =
      let loc =
        { Location.
          loc_start ;
          loc_end = shift loc_start (String.length source) ;
          loc_ghost = false ;
        } in
      Location.mkloc source loc
    in
    let len = String.length expr in
    let rec aux acc i =
      if i >= len then
        List.rev_map ~f:add_loc (expr :: acc)
      else if expr.[i] = '.' then
        aux (String.sub expr ~pos:0 ~len:i :: acc) (succ i)
      else
        aux acc (succ i) in
    aux [] offset

let dispatch pipeline (type a) : a Query_protocol.t -> a =
  function
  | Type_expr (source, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
    let ppf, to_string = Format.to_string () in
    let verbosity = verbosity pipeline in
    let context = Context.Expr in
    ignore (Type_utils.type_in_env ~verbosity ~context env ppf source : bool);
    to_string ()

  | Type_enclosing (expro, pos, index) ->
    let typer = Mpipeline.typer_result pipeline in
    let verbosity = verbosity pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let structures = Mbrowse.enclosing pos
      [Mbrowse.of_typedtree (Mtyper.get_typedtree typer)] in
    let path = match structures with
      | [] -> []
      | browse -> Browse_misc.annotate_tail_calls browse
    in
    let exprs = reconstruct_identifier pipeline pos expro in

    let () =
      Logger.log ~section:Type_enclosing.log_section
        ~title:"reconstruct identifier" "%a"
        Logger.json (fun () ->
          let lst =
            List.map exprs ~f:(fun { Location.loc; txt } ->
              `Assoc [ "start", Lexing.json_of_position loc.Location.loc_start
                     ; "end",   Lexing.json_of_position loc.Location.loc_end
                     ; "identifier", `String txt]
            )
          in
          `List lst
        )
    in

    let result = Type_enclosing.from_nodes ~path in

    (* enclosings of cursor in given expression *)
    let exprs = reconstruct_identifier pipeline pos expro in
    let () =
      Logger.log ~section:Type_enclosing.log_section
        ~title:"reconstruct identifier" "%a"
        Logger.json (fun () ->
          let lst =
            List.map exprs ~f:(fun { Location.loc; txt } ->
              `Assoc [ "start", Lexing.json_of_position loc.Location.loc_start
                     ; "end",   Lexing.json_of_position loc.Location.loc_end
                     ; "identifier", `String txt]
            )
          in
          `List lst
        )
    in
    let nodes = Mtyper.node_at typer pos in
    let small_enclosings =
      Type_enclosing.from_reconstructed exprs
       ~nodes ~cursor:pos ~verbosity
    in
    Logger.log ~section:Type_enclosing.log_section ~title:"small enclosing" "%a"
      Logger.fmt (fun fmt ->
        Format.fprintf fmt "result = [ %a ]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun fmt (loc, _, _) -> Location.print_loc fmt loc))
          small_enclosings
      );

    let all_items =
      let normalize ({Location. loc_start; loc_end; _}, text, _tail) =
        Lexing.split_pos loc_start, Lexing.split_pos loc_end, text in
      List.merge_cons
        ~f:(fun a b ->
            (* Tail position is computed only on result, and result comes last
               As an approximation, when two items are similar, we returns the
               rightmost one *)
            if compare (normalize a) (normalize b) = 0 then Some b else None)

        (small_enclosings @ result)
    in
    let ppf = Format.str_formatter in
    List.mapi all_items
      ~f:(fun i (loc,text,tail) ->
          let print = match index with None -> true | Some index -> index = i in
          let ret x = (loc, x, tail) in
          match text with
          | Type_enclosing.String str -> ret (`String str)
          | Type_enclosing.Type (env, t) when print ->
            Printtyp.wrap_printing_env env ~verbosity
              (fun () -> Type_utils.print_type_with_decl ~verbosity env ppf t);
            ret (`String (Format.flush_str_formatter ()))
          | Type_enclosing.Type_decl (env, id, t) when print ->
            Printtyp.wrap_printing_env env ~verbosity
              (fun () -> Printtyp.type_declaration env id ppf t);
            ret (`String (Format.flush_str_formatter ()))
          | Type_enclosing.Modtype (env, m) when print ->
            Printtyp.wrap_printing_env env ~verbosity
              (fun () -> Printtyp.modtype env ppf m);
            ret (`String (Format.flush_str_formatter ()))
          | _ -> ret (`Index i)
        )

  | Enclosing pos ->
    let typer = Mpipeline.typer_result pipeline in
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let path = match Mbrowse.enclosing pos [structures] with
      | [] -> []
      | path -> List.map ~f:snd path
    in
    List.map ~f:Mbrowse.node_loc path

  | Locate_type pos ->
    let typer = Mpipeline.typer_result pipeline in
    let structures = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let node =
      match Mbrowse.enclosing pos [structures] with
      | path :: _ -> Some path
      | [] -> None
    in
    let path =
      Option.bind node ~f:(fun (env, node) ->
          Locate.log ~title:"query_commands Locate_type"
            "inspecting node: %s" (Browse_raw.string_of_node node);
          match node with
          | Browse_raw.Expression {exp_type = ty; _}
          | Pattern {pat_type = ty; _}
          | Core_type {ctyp_type = ty; _}
          | Value_description { val_desc = { ctyp_type = ty; _ }; _ } ->
            begin match (Ctype.repr ty).desc with
              | Tconstr (path, _, _) -> Some (env, path)
              | _ -> None
            end
          | _ -> None)
    in
    begin match path with
      | None -> `Invalid_context
      | Some (env, path) ->
        Locate.log ~title:"debug" "found type: %s" (Path.name path);
        let local_defs = Mtyper.get_typedtree typer in
        match Locate.from_path
                ~env
                ~config:(Mpipeline.final_config pipeline)
                ~local_defs ~pos ~namespace:`Type `MLI
                path with
        | `Builtin -> `Builtin (Path.name path)
        | `Not_in_env _ as s -> s
        | `Not_found _ as s -> s
        | `Found _ as s -> s
        | `File_not_found _ as s -> s
    end

  | Complete_prefix (prefix, pos, kinds, with_doc, with_types) ->
    let pipeline, typer = for_completion pipeline pos in
    let config = Mpipeline.final_config pipeline in
    let verbosity = Mconfig.(config.query.verbosity) in
    let no_labels = Mpipeline.reader_no_labels_for_completion pipeline in
    let source = Mpipeline.input_source pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let branch = Mtyper.node_at ~skip_recovered:true typer pos in
    let env, _ = Mbrowse.leaf_node branch in
    let target_type, context =
      Completion.application_context ~prefix branch in
    let get_doc =
      if not with_doc then None else
        let local_defs = Mtyper.get_typedtree typer in
        Some (Locate.get_doc ~config ~env ~local_defs
                ~comments:(Mpipeline.reader_comments pipeline) ~pos)
    in
    let entries =
      Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
      Completion.branch_complete config ~kinds ?get_doc ?target_type prefix branch |>
      print_completion_entries ~with_types config source
    and context = match context with
      | `Application context when no_labels ->
        `Application {context with Compl.labels = []}
      | context -> context
    in
    {Compl. entries; context }

  | Expand_prefix (prefix, pos, kinds, with_types) ->
    let pipeline, typer = for_completion pipeline pos in
    let source = Mpipeline.input_source pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
    let config = Mpipeline.final_config pipeline in
    let global_modules = Mconfig.global_modules config in
    let entries =
      Completion.expand_prefix env ~global_modules ~kinds prefix |>
      print_completion_entries ~with_types config source
    in
    { Compl. entries ; context = `Unknown }

  | Polarity_search (query, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
    let query =
      let re = Str.regexp "[ |\t]+" in
      let pos,neg = Str.split re query |> List.partition ~f:(fun s->s.[0]<>'-') in
      let prepare s =
        Longident.parse @@
        if s.[0] = '-' || s.[0] = '+'
        then String.sub s ~pos:1 ~len:(String.length s - 1)
        else s
      in
      Polarity_search.build_query env
        ~positive:(List.map pos ~f:prepare)
        ~negative:(List.map neg ~f:prepare)
    in
    let config = Mpipeline.final_config pipeline in
    let global_modules = Mconfig.global_modules config in
    let dirs = Polarity_search.directories ~global_modules env in
    ignore (Format.flush_str_formatter ());
    let entries =
      Polarity_search.execute_query query env dirs |>
      List.sort ~cmp:compare |>
      Printtyp.wrap_printing_env env ~verbosity:(verbosity pipeline) @@ fun () ->
      List.map ~f:(fun (_, path, v) ->
          Printtyp.path Format.str_formatter path;
          let name = Format.flush_str_formatter () in
          Printtyp.type_scheme env Format.str_formatter v.Types.val_type;
          let desc = Format.flush_str_formatter () in
          {Compl. name; kind = `Value; desc; info = ""; deprecated = false }
        )
    in
    { Compl. entries ; context = `Unknown }

  | Refactor_open (mode, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    begin match Raw_compat.select_open_node (Mtyper.node_at typer pos) with
      | None | Some (_, []) -> []
      | Some (path, ((_, node) :: _)) ->
        let paths =
          Browse_tree.all_occurrences_of_prefix ~strict_prefix:true path node in
        let paths = List.concat_map ~f:snd paths in
        let rec path_to_string acc p =
          match Path.Nopos.view p with
          | Pident ident ->
            String.concat ~sep:"." (Ident.name ident :: acc)
          | Pdot (path', s) when
              mode = `Unqualify && Path.same path path' ->
            String.concat ~sep:"." (s :: acc)
          | Pdot (path', s) ->
            path_to_string (s :: acc) path'
          | _ -> raise Not_found
        in
        List.filter_map paths ~f:(fun {Location. txt = path; loc} ->
            if not loc.Location.loc_ghost &&
               Location_aux.compare_pos pos loc <= 0 then
              try Some (path_to_string [] path, loc)
              with Not_found -> None
            else None
          )
        |> List.sort
          ~cmp:(fun (_,l1) (_,l2) ->
              Lexing.compare_pos l1.Location.loc_start l2.Location.loc_start)
    end

  | Document (patho, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let local_defs = Mtyper.get_typedtree typer in
    let config = Mpipeline.final_config pipeline in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let comments = Mpipeline.reader_comments pipeline in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let path = reconstruct_identifier pipeline pos None in
        let path = Mreader_lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt; _} -> txt) path in
        String.concat ~sep:"." path
    in
    if path = "" then `Invalid_context else
      Locate.get_doc ~config
        ~env ~local_defs ~comments ~pos (`User_input path)

  | Locate (patho, ml_or_mli, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let local_defs = Mtyper.get_typedtree typer in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let env, _ = Mbrowse.leaf_node (Mtyper.node_at typer pos) in
    let path =
      match patho with
      | Some p -> p
      | None ->
        let path = reconstruct_identifier pipeline pos None in
        let path = Mreader_lexer.identifier_suffix path in
        let path = List.map ~f:(fun {Location. txt; _} -> txt) path in
        let path = String.concat ~sep:"." path in
        Locate.log ~title:"reconstructed identifier" "%s" path;
        path
    in
    if path = "" then `Invalid_context else
    begin match
      Locate.from_string
        ~config:(Mpipeline.final_config pipeline)
        ~env ~local_defs ~pos ml_or_mli path
    with
    | `Found (file, pos) ->
      Locate.log ~title:"result"
        "found: %s" (Option.value ~default:"<local buffer>" file);
      `Found (file, pos)
    | `Missing_labels_namespace ->
      (* Can't happen because we haven't passed a namespace as input. *)
      assert false
    | (`Not_found _|`At_origin |`Not_in_env _|`File_not_found _|`Builtin _) as
      otherwise ->
      Locate.log ~title:"result" "not found";
      otherwise
    end

  | Jump (target, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let typedtree = Mtyper.get_typedtree typer in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    Jump.get typedtree pos target

  | Phrase (target, pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let typedtree = Mtyper.get_typedtree typer in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    Mpipeline.get_lexing_pos pipeline (Jump.phrase typedtree pos target)

  | Case_analysis (pos_start, pos_end) ->
    let typer = Mpipeline.typer_result pipeline in
    let pos_start = Mpipeline.get_lexing_pos pipeline pos_start in
    let pos_end = Mpipeline.get_lexing_pos pipeline pos_end in
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let nodes = Mbrowse.enclosing pos_start [browse] in
    let dump_node (_,node) =
      let {Location. loc_start; loc_end; _} =
        Mbrowse.node_loc node in
      let l1,c1 = Lexing.split_pos loc_start in
      let l2,c2 = Lexing.split_pos loc_end in
      `List [
         `String (Browse_raw.string_of_node node);
         `Int l1; `Int c1;
         `Int l2; `Int c2;
       ]
    in
    Destruct.log ~title:"nodes before" "%a"
      Logger.json (fun () -> `List (List.map nodes ~f:dump_node));
    let nodes =
      (* Drop nodes that:
         - start inside the user's selection
         - finish inside the user's selection
      *)
      List.drop_while nodes
        ~f:(fun (_,t) ->
          let {Location. loc_start; loc_end; _} = Mbrowse.node_loc t in
          Lexing.compare_pos loc_start pos_start > 0 || Lexing.compare_pos loc_end pos_end < 0)
    in
    Destruct.log ~title:"nodes after" "%a"
      Logger.json (fun () -> `List (List.map nodes ~f:dump_node));
    begin match nodes with
      | [] -> raise No_nodes
      | (env,node) :: parents ->
        let source = Mpipeline.input_source pipeline in
        let config = Mpipeline.final_config pipeline in
        let verbosity = verbosity pipeline in
        Printtyp.wrap_printing_env env ~verbosity @@ fun () ->
        Destruct.node config source node (List.map ~f:snd parents)
    end

  | Outline ->
    let typer = Mpipeline.typer_result pipeline in
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    Outline.get [Browse_tree.of_browse browse]

  | Shape pos ->
    let typer = Mpipeline.typer_result pipeline in
    let browse = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    Outline.shape pos [Browse_tree.of_browse browse]

  | Errors { lexing; parsing; typing }->
    let typer = Mpipeline.typer_result pipeline in
    let verbosity = verbosity pipeline in
    Printtyp.wrap_printing_env (Mtyper.get_env typer) ~verbosity @@ fun () ->
    let lexer_errors  = Mpipeline.reader_lexer_errors pipeline  in
    let parser_errors = Mpipeline.reader_parser_errors pipeline in
    let typer_errors  = Mpipeline.typer_errors pipeline  in
    (* When there is a cmi error, we will have a lot of meaningless errors,
       there is no need to report them. *)
    let typer_errors =
      let cmi_error = function Magic_numbers.Cmi.Error _ -> true | _ -> false in
      match List.find typer_errors ~f:cmi_error with
      | e -> [e]
      | exception Not_found -> typer_errors
    in
    let error_start e = (Location.loc_of_report e).Location.loc_start in
    let error_end e = (Location.loc_of_report e).Location.loc_end in
    (* Turn into Location.error, ignore ghost warnings *)
    let filter_error exn =
      match Location.error_of_exn exn with
      | None | Some `Already_displayed -> None
      | Some (`Ok (err : Location.error)) ->
        if (Location.loc_of_report err).loc_ghost &&
           (match exn with Msupport.Warning _ -> true | _ -> false)
        then None
        else Some err
    in
    let lexer_errors  = List.filter_map ~f:filter_error lexer_errors in
    (* Ast can contain syntax error *)
    let first_syntax_error = ref Lexing.dummy_pos in
    let filter_typer_error exn =
      let result = filter_error exn in
      begin match result with
        | Some ({Location. source = Location.Parser; _} as err)
          when !first_syntax_error = Lexing.dummy_pos ||
               Lexing.compare_pos !first_syntax_error (error_start err) > 0 ->
          first_syntax_error := error_start err;
        | _ -> ()
      end;
      result
    in
    let typer_errors  = List.filter_map ~f:filter_typer_error typer_errors in
    (* Track first parsing error *)
    let filter_parser_error = function
      | Msupport.Warning _ as exn -> filter_error exn
      | exn ->
        let result = filter_error exn in
        begin match result with
          | None -> ()
          | Some err ->
            if !first_syntax_error = Lexing.dummy_pos ||
               Lexing.compare_pos !first_syntax_error (error_start err) > 0
            then first_syntax_error := error_start err;
        end;
        result
    in
    let parser_errors = List.filter_map ~f:filter_parser_error parser_errors in
    (* Sort errors *)
    let cmp e1 e2 =
      let n = Lexing.compare_pos (error_start e1) (error_start e2) in
      if n <> 0 then n else
        Lexing.compare_pos (error_end e1) (error_end e2)
    in
    let errors =
      List.sort_uniq ~cmp
        ((if lexing then lexer_errors else []) @
         (if parsing then parser_errors else []) @
         (if typing then typer_errors else []))
    in
    (* Add configuration errors *)
    let errors =
      let cfg = Mpipeline.final_config pipeline in
      let failures =
        List.map ~f:(Location.error ~source:Location.Config) cfg.merlin.failures
      in
      failures @ errors
    in
    (* Filter anything after first parse error *)
    let limit = !first_syntax_error in
    if limit = Lexing.dummy_pos then errors else (
      List.take_while errors
        ~f:(fun err -> Lexing.compare_pos (error_start err) limit <= 0)
    )

  | Dump args -> dump pipeline args

  | Path_of_source xs ->
    let config = Mpipeline.final_config pipeline in
    let rec aux = function
      | [] -> raise Not_found
      | x :: xs ->
        try
          find_in_path_uncap (Mconfig.source_path config) x
        with Not_found -> try
            find_in_path_uncap (Mconfig.build_path config) x
          with Not_found ->
            aux xs
    in
    aux xs

  | List_modules exts ->
    let config = Mpipeline.final_config pipeline in
    let with_ext ext = modules_in_path ~ext
        Mconfig.(config.merlin.source_path) in
    List.concat_map ~f:with_ext exts

  | Findlib_list ->
    []

  | Extension_list kind ->
    let config = Mpipeline.final_config pipeline in
    let enabled = Mconfig.(config.merlin.extensions) in
    begin match kind with
    | `All -> Extension.all
    | `Enabled -> enabled
    | `Disabled ->
      List.fold_left ~f:(fun exts ext -> List.remove ext exts)
        ~init:Extension.all enabled
    end

  | Path_list `Build ->
    let config = Mpipeline.final_config pipeline in
    Mconfig.(config.merlin.build_path)

  | Path_list `Source ->
    let config = Mpipeline.final_config pipeline in
    Mconfig.(config.merlin.source_path)

  | Occurrences (`Ident_at pos) ->
    let typer = Mpipeline.typer_result pipeline in
    let str = Mbrowse.of_typedtree (Mtyper.get_typedtree typer) in
    let pos = Mpipeline.get_lexing_pos pipeline pos in
    let tnode =
      let should_ignore_tnode = function
        | Browse_raw.Pattern {pat_desc = Typedtree.Tpat_any; _} -> true
        | _ -> false
      in
      let rec find = function
        | [] -> Browse_tree.dummy
        | (env, node)::rest ->
          if should_ignore_tnode node
          then find rest
          else Browse_tree.of_node ~env node
      in
      find (Mbrowse.enclosing pos [str])
    in
    let str = Browse_tree.of_browse str in
    let get_loc {Location.txt = _; loc} = loc in
    let ident_occurrence () =
      let paths = Browse_raw.node_paths tnode.Browse_tree.t_node in
      let under_cursor p = Location_aux.compare_pos pos (get_loc p) = 0 in
      Logger.log ~section:"occurrences" ~title:"Occurrences paths" "%a"
        Logger.json (fun () ->
            let dump_path ({Location.txt; loc} as p) =
              let ppf, to_string = Format.to_string () in
              Printtyp.path ppf txt;
              `Assoc [
                "start", Lexing.json_of_position loc.Location.loc_start;
                "end", Lexing.json_of_position loc.Location.loc_end;
                "under_cursor", `Bool (under_cursor p);
                "path", `String (to_string ())
              ]
            in
            `List (List.map ~f:dump_path paths));
      match List.filter paths ~f:under_cursor with
      | [] -> []
      | (path :: _) ->
        let path = path.Location.txt in
        let ts = Browse_tree.all_occurrences path str in
        let loc (_t,paths) = List.map ~f:get_loc paths in
        List.concat_map ~f:loc ts

    and constructor_occurrence d =
      let ts = Browse_tree.all_constructor_occurrences (tnode,d) str in
      List.map ~f:get_loc ts

    in
    let locs =
      match Browse_raw.node_is_constructor tnode.Browse_tree.t_node with
      | Some d -> constructor_occurrence d.Location.txt
      | None -> ident_occurrence ()
    in
    let loc_start l = l.Location.loc_start in
    let cmp l1 l2 = Lexing.compare_pos (loc_start l1) (loc_start l2) in
    List.sort ~cmp locs

  | Version ->
    Printf.sprintf "The Merlin toolkit version %s, for Ocaml %s\n"
      My_config.version Sys.ocaml_version;
