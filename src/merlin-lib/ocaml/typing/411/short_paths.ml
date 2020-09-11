
open Short_paths_graph

module Desc = Desc

module Rev_deps : sig

  type t

  val create : unit -> t

  val extend_up_to : t -> Dependency.t -> unit

  val get : t -> Dependency.t -> Dependency.Set.t

  val add : t -> source:Dependency.t -> target:Dependency.t -> unit

  val add_alias : t -> source:Dependency.t -> target:Dependency.t -> unit

  val before : t -> Origin.t -> Origin.t -> bool

end = struct

  module Stamp = Natural.Make()

  type item =
    { mutable set : Dependency.Set.t;
      mutable edges : Dependency.t list;
      mutable alias_edges : Dependency.t list;
      mutable last : Stamp.t; }

  type t =
    { mutable stamp : Stamp.t;
      mutable items : item Dependency.Array.t; }

  let create () =
    { stamp = Stamp.one;
      items = Dependency.Array.empty; }

  let extend_up_to t next =
    match Dependency.pred next with
    | None -> ()
    | Some curr ->
      if not (Dependency.Array.contains t.items curr) then begin
        let items =
          Dependency.Array.extend t.items curr
            (fun _ -> { set = Dependency.Set.empty;
                        edges = [];
                        alias_edges = [];
                        last = Stamp.zero; })
        in
        t.items <- items
      end

  let add t ~source ~target =
    let item = Dependency.Array.get t.items source in
    item.edges <- target :: item.edges;
    t.stamp <- Stamp.succ t.stamp

  let add_alias t ~source ~target =
    let item = Dependency.Array.get t.items source in
    item.alias_edges <- target :: item.alias_edges;
    t.stamp <- Stamp.succ t.stamp

  let update t dep item =
    if Stamp.less_than item.last t.stamp then begin
      let rec add_edges t item acc =
        let rec loop t acc added = function
          | [] ->
              List.fold_left
                (fun acc dep ->
                   let item = Dependency.Array.get t.items dep in
                   add_alias_edges t item acc)
                acc added
          | dep :: rest ->
              if Dependency.Set.mem dep acc then loop t acc added rest
              else begin
                let acc = Dependency.Set.add dep acc in
                let added = dep :: added in
                loop t acc added rest
              end
        in
        loop t acc [] item.edges
      and add_alias_edges t item acc =
        List.fold_left
          (fun acc dep ->
             if Dependency.Set.mem dep acc then acc
             else begin
               let acc = Dependency.Set.add dep acc in
               let item = Dependency.Array.get t.items dep in
               let acc = add_edges t item acc in
               add_alias_edges t item acc
             end)
          acc item.alias_edges
      in
      let set = Dependency.Set.singleton dep in
      let set = add_edges t item set in
      let set = add_alias_edges t item set in
      item.set <- set;
      item.last <- t.stamp
    end

  let get t dep =
    let item = Dependency.Array.get t.items dep in
    update t dep item;
    item.set

  let before t origin1 origin2 =
    let open Origin in
    match origin1, origin2 with
    | Environment age1, Environment age2 -> Age.less_than age1 age2
    | Environment _, Dependency _ -> false
    | Environment _, Dependencies _ -> false
    | Dependency _, Environment _ -> true
    | Dependency dep1, Dependency dep2 ->
        let rev_dep = get t dep1 in
        Dependency.Set.mem dep2 rev_dep
    | Dependency dep1, Dependencies deps2 ->
        let rev_dep = get t dep1 in
        List.exists
          (fun dep2 -> Dependency.Set.mem dep2 rev_dep)
          deps2
    | Dependencies _, Environment _ -> true
    | Dependencies deps1, Dependency dep2 ->
        List.for_all
          (fun dep1 -> Dependency.Set.mem dep2 (get t dep1))
          deps1
    | Dependencies deps1, Dependencies deps2 ->
        let rev_dep =
          match deps1 with
          | [] -> failwith "Rev_deps.before: invalid origin"
          | dep1 :: deps1 ->
              List.fold_left
                (fun acc dep1 -> Dependency.Set.inter acc (get t dep1))
                (get t dep1) deps1
        in
        List.exists
          (fun dep2 -> Dependency.Set.mem dep2 rev_dep)
          deps2

end

module Origin_range_tbl = struct

  type 'a t =
    { mutable envs : 'a list Age.Map.t;
      mutable dep_keys : Dependency.Set.t;
      deps : 'a list Dependency.Tbl.t; }

  let create () =
    { envs = Age.Map.empty;
      dep_keys = Dependency.Set.empty;
      deps = Dependency.Tbl.create 0; }

  let add_dependency dep data t =
    t.dep_keys <- Dependency.Set.add dep t.dep_keys;
    let prev =
      match Dependency.Tbl.find t.deps dep with
      | exception Not_found -> []
      | prev -> prev
    in
    Dependency.Tbl.replace t.deps dep (data :: prev)

  let add_age age data t =
    let prev =
      match Age.Map.find age t.envs with
      | exception Not_found -> []
      | prev -> prev
    in
    t.envs <- Age.Map.add age (data :: prev) t.envs

  let add rev_deps origin data t =
    match origin with
    | Origin.Dependency dep -> add_dependency dep data t
    | Origin.Environment age -> add_age age data t
    | Origin.Dependencies deps -> begin
        let rev_dep_opt =
          List.fold_left
            (fun acc dep ->
               let rev_dep = Rev_deps.get rev_deps dep in
               match acc with
               | None -> Some rev_dep
               | Some acc -> Some (Dependency.Set.inter acc rev_dep))
            None deps
        in
        let rev_dep =
          match rev_dep_opt with
          | None -> failwith "Origin_range_tbl.add: invalid origin"
          | Some rev_dep -> rev_dep
        in
        match
          List.find
            (fun dep -> Dependency.Set.mem dep rev_dep)
            deps
        with
        | dep -> add_dependency dep data t
        | exception Not_found ->
          match Dependency.Set.choose rev_dep with
          | dep -> add_dependency dep data t
          | exception Not_found -> add_age Age.zero data t
      end

  let pop_dependency rev_dep t =
    let matching = Dependency.Set.inter rev_dep t.dep_keys in
    t.dep_keys <- Dependency.Set.diff t.dep_keys matching;
    let items =
      Dependency.Set.fold
        (fun dep acc ->
           let data = Dependency.Tbl.find t.deps dep in
           Dependency.Tbl.remove t.deps dep;
           List.rev_append data acc)
        matching
        []
    in
    let items =
      Age.Map.fold
        (fun _ data acc -> List.rev_append data acc)
        t.envs items
    in
    t.envs <- Age.Map.empty;
    items

  let pop_age age t =
    let envs, first, matching = Age.Map.split age t.envs in
    let items =
      match first with
      | None -> []
      | Some first -> first
    in
    let items =
      Age.Map.fold
        (fun _ data acc -> List.rev_append data acc)
        matching items
    in
    t.envs <- envs;
    items

  let pop rev_deps origin t =
    match origin with
    | Origin.Dependency dep ->
        let rev_dep = Rev_deps.get rev_deps dep in
        pop_dependency rev_dep t
    | Origin.Dependencies deps ->
        let rev_dep_opt =
          List.fold_left
            (fun acc dep ->
               let rev_dep = Rev_deps.get rev_deps dep in
               match acc with
               | None -> Some rev_dep
               | Some acc -> Some (Dependency.Set.inter acc rev_dep))
            None deps
        in
        let rev_dep =
          match rev_dep_opt with
          | None -> failwith "Origin_range_tbl.pop: invalid origin"
          | Some rev_dep -> rev_dep
        in
        pop_dependency rev_dep t
    | Origin.Environment age ->
        pop_age age t

  let is_origin_empty rev_deps origin t =
    match origin with
    | Origin.Dependency dep ->
        if not (Age.Map.is_empty t.envs) then false
        else begin
          let rev_dep = Rev_deps.get rev_deps dep in
          let matching = Dependency.Set.inter rev_dep t.dep_keys in
          Dependency.Set.is_empty matching
        end
    | Origin.Dependencies deps ->
        if not (Age.Map.is_empty t.envs) then false
        else begin
          let rev_dep_opt =
            List.fold_left
              (fun acc dep ->
                 let rev_dep = Rev_deps.get rev_deps dep in
                 match acc with
                 | None -> Some rev_dep
                 | Some acc -> Some (Dependency.Set.inter acc rev_dep))
              None deps
          in
          let rev_dep =
            match rev_dep_opt with
            | None ->
                failwith "Origin_range_tbl.is_origin_empty: invalid origin"
            | Some rev_dep -> rev_dep
          in
          let matching = Dependency.Set.inter rev_dep t.dep_keys in
          Dependency.Set.is_empty matching
        end
    | Origin.Environment age ->
        match Age.Map.max_binding t.envs with
        | exception Not_found -> true
        | (max, _) -> Age.less_than max age

  let is_completely_empty t =
    Age.Map.is_empty t.envs
    && Dependency.Set.is_empty t.dep_keys

end

module Height = Natural.Make_no_zero()

module Todo = struct

  module Item = struct

    type t =
      | Base of Diff.Item.t
      | Children of
          { md : Module.t;
            path : Path.t;
            seen : Path_set.t; }
      | Update of
          { id : Ident.t;
            origin : Origin.t; }
      | Forward of
          { id : Ident.t;
            decl : Origin.t;
            origin : Origin.t; }

  end

  type t =
    { mutable table : Item.t Origin_range_tbl.t Height.Array.t }

  let create graph rev_deps diff =
    let tbl = Origin_range_tbl.create () in
    List.iter
      (fun item ->
         let origin = Diff.Item.origin graph item in
         match Diff.Item.previous graph item with
         | None ->
             Origin_range_tbl.add rev_deps origin (Item.Base item) tbl;
         | Some decl ->
             let id = Diff.Item.id graph item in
             let item = Item.Forward { id; decl; origin } in
             Origin_range_tbl.add rev_deps origin item tbl)
      diff;
    let table = Height.Array.singleton tbl in
    { table }

  let get_table t height =
    if not (Height.Array.contains t.table height) then begin
      t.table <- Height.Array.extend t.table height
                   (fun _ -> Origin_range_tbl.create ());
    end;
    Height.Array.get t.table height

  let get_table_opt t height =
    if Height.Array.contains t.table height then
      Some (Height.Array.get t.table height)
    else None

  let retract_empty t =
    let rec loop height =
      match Height.pred height with
      | None ->
          t.table <- Height.Array.empty
      | Some prev ->
          let tbl = Height.Array.get t.table prev in
          if Origin_range_tbl.is_completely_empty tbl then loop prev
          else begin
            t.table <- Height.Array.retract t.table height
          end
    in
    match Height.Array.last t.table with
    | None -> ()
    | Some last ->
      let tbl = Height.Array.get t.table last in
      if Origin_range_tbl.is_completely_empty tbl then loop last
      else ()

  let merge graph rev_deps t diff =
    let tbl = get_table t Height.one in
    List.iter
      (fun item ->
         match Diff.Item.previous graph item with
         | None -> ()
         | Some origin ->
             let id = Diff.Item.id graph item in
             let item = Item.Update { id; origin } in
             Origin_range_tbl.add rev_deps origin item tbl)
      diff

  let mutate graph rev_deps t diff =
    let tbl = get_table t Height.one in
    List.iter
      (fun item ->
         match Diff.Item.previous graph item with
         | None ->
             let origin = Diff.Item.origin graph item in
             Origin_range_tbl.add rev_deps origin (Item.Base item) tbl;
         | Some origin ->
             let id = Diff.Item.id graph item in
             let item = Item.Update { id; origin } in
             Origin_range_tbl.add rev_deps origin item tbl)
      diff

  let add_children graph rev_deps t height md path seen =
    let height = Height.succ height in
    let tbl = get_table t height in
    let origin = Module.origin graph md in
    Origin_range_tbl.add rev_deps origin (Item.Children{md; path; seen}) tbl

  let add_next_update rev_deps t height origin id =
    let height = Height.succ height in
    let tbl = get_table t height in
    let item = Item.Update { id; origin } in
    Origin_range_tbl.add rev_deps origin item tbl

  let add_next_forward rev_deps t height origin id decl =
    let height = Height.succ height in
    let tbl = get_table t height in
    let item = Item.Forward { id; decl; origin } in
    Origin_range_tbl.add rev_deps origin item tbl

  let rec is_empty_from rev_deps t height origin =
    match get_table_opt t height with
    | None -> true
    | Some tbl ->
        Origin_range_tbl.is_origin_empty rev_deps origin tbl
        && is_empty_from rev_deps t (Height.succ height) origin

  let pop rev_deps t height origin =
    match get_table_opt t height with
    | None ->
        retract_empty t;
        None
    | Some tbl ->
      match Origin_range_tbl.pop rev_deps origin tbl with
      | [] ->
          let empty_from =
            is_empty_from rev_deps t (Height.succ height) origin
          in
          if not empty_from then Some []
          else begin
            retract_empty t;
            None
          end
      | _ :: _ as todo -> Some todo

end

module Forward_path_map : sig

  type 'a t

  val empty : 'a t

  val add : 'a t -> Sort.t -> Path.t -> 'a -> 'a t

  val find : 'a t -> Path.t -> 'a list

  val rebase : 'a t -> 'a t -> 'a t

  val iter_forwards : (Path.t -> 'a -> unit) -> 'a t -> Ident.t -> unit

  val iter_updates : (Path.t -> 'a -> unit) -> 'a t -> Ident.t -> unit

end = struct

  type 'a t =
    { new_paths : 'a list Path_map.t;
      old_paths : 'a list Path_map.t;
      updates : Path_set.t Ident_map.t;
      forwards : Path_set.t Ident_map.t; }

  let empty =
    { new_paths = Path_map.empty;
      old_paths = Path_map.empty;
      forwards = Ident_map.empty;
      updates = Ident_map.empty; }

  let add t sort path data =
    let new_paths = t.new_paths in
    let prev =
      match Path_map.find path new_paths with
      | prev -> prev
      | exception Not_found -> []
    in
    let new_paths = Path_map.add path (data :: prev) new_paths in
    let updates = t.updates in
    let updates =
      match sort with
      | Sort.Defined -> updates
      | Sort.Declared ids ->
          Ident_set.fold
            (fun id acc ->
               let prev =
                 match Ident_map.find id updates with
                 | prev -> prev
                 | exception Not_found -> Path_set.empty
               in
               Ident_map.add id (Path_set.add path prev) acc)
            ids updates
    in
    { t with new_paths; updates }

  let find t path =
    match Path_map.find path t.new_paths with
    | exception Not_found -> Path_map.find path t.old_paths
    | new_paths ->
      match Path_map.find path t.old_paths with
      | exception Not_found -> new_paths
      | old_paths -> new_paths @ old_paths

  let rebase t base =
    let old_paths =
      Path_map.union
        (fun _ paths1 paths2 -> Some (paths1 @ paths2))
        base.new_paths base.old_paths
    in
    let forwards =
      Ident_map.union
        (fun _ pset1 pset2 -> Some (Path_set.union pset1 pset2))
        base.updates base.forwards
    in
    { t with old_paths; forwards }

  let iter_updates f t id =
    match Ident_map.find id t.updates with
    | exception Not_found -> ()
    | pset ->
        Path_set.iter
          (fun path ->
             match Path_map.find path t.new_paths with
             | exception Not_found -> ()
             | paths -> List.iter (f path) paths)
          pset

  let iter_forwards f t id =
    match Ident_map.find id t.forwards with
    | exception Not_found -> ()
    | pset ->
        Path_set.iter
          (fun path ->
             match Path_map.find path t.old_paths with
             | exception Not_found -> ()
             | paths -> List.iter (f path) paths)
          pset

end

module Origin_tbl = Hashtbl.Make(Origin)

module History : sig

  module Stamp : Natural.S

  module Revision : sig

    type t

    val stamp : t -> Stamp.t

    val diff : t -> Diff.t

    val rev_deps : t -> Rev_deps.t

    val next : t -> t option

  end

  type t

  val init : Rev_deps.t ->  Diff.t -> t

  val head : t -> Revision.t

  val commit : t -> Rev_deps.t ->  Diff.t -> unit

end = struct

  module Stamp = Natural.Make()

  module Revision = struct

    type t =
      { stamp : Stamp.t;
        diff :  Diff.t;
        rev_deps : Rev_deps.t;
        mutable next : t option; }

    let stamp t = t.stamp

    let diff t = t.diff

    let rev_deps t = t.rev_deps

    let next t = t.next

  end

  type t =
    { mutable head : Revision.t; }

  let init rev_deps diff =
    let stamp = Stamp.zero in
    let next = None in
    let head = { Revision.stamp; diff; rev_deps; next } in
    { head }

  let head t = t.head

  let commit t rev_deps diff =
    let head = t.head in
    let stamp = Stamp.succ head.Revision.stamp in
    let next = None in
    let rev = { Revision.stamp; diff; rev_deps; next } in
    head.Revision.next <- Some rev;
    t.head <- rev

end

type type_resolution =
  | Nth of int
  | Subst of int list
  | Id

type type_result =
  | Nth of int
  | Path of int list option * Path.t

type class_type_result = int list option * Path.t

module Shortest = struct

  module Section = struct

    type t =
      { mutable types : Path.t Forward_path_map.t;
        mutable class_types : Path.t Forward_path_map.t;
        mutable module_types : Path.t Forward_path_map.t;
        mutable modules : (Path.t * Path_set.t) Forward_path_map.t; }

    let create () =
      let types = Forward_path_map.empty in
      let class_types = Forward_path_map.empty in
      let module_types = Forward_path_map.empty in
      let modules = Forward_path_map.empty in
      { types; class_types; module_types; modules }

    let add_type graph t typ path =
      let canonical = Type.path graph typ in
      let sort = Type.sort graph typ in
      t.types <- Forward_path_map.add t.types sort canonical path

    let add_class_type graph t mty path =
      let canonical = Class_type.path graph mty in
      let sort = Class_type.sort graph mty in
      t.class_types <- Forward_path_map.add t.class_types sort canonical path

    let add_module_type graph t mty path =
      let canonical = Module_type.path graph mty in
      let sort = Module_type.sort graph mty in
      t.module_types <- Forward_path_map.add t.module_types sort canonical path

    let add_module graph t md path =
      let canonical = Module.path graph md in
      let sort = Module.sort graph md in
      t.modules <- Forward_path_map.add t.modules sort canonical path

    let rebase t parent =
      t.types <- Forward_path_map.rebase t.types parent.types;
      t.class_types <- Forward_path_map.rebase t.class_types parent.class_types;
      t.module_types <- Forward_path_map.rebase t.module_types parent.module_types;
      t.modules <- Forward_path_map.rebase t.modules parent.modules

    let iter_updates ~type_ ~class_type ~module_type ~module_ t id =
      Forward_path_map.iter_updates type_ t.types id;
      Forward_path_map.iter_updates class_type t.class_types id;
      Forward_path_map.iter_updates module_type t.module_types id;
      Forward_path_map.iter_updates module_ t.modules id

    let iter_forwards ~type_ ~class_type ~module_type ~module_ t id =
      Forward_path_map.iter_forwards type_ t.types id;
      Forward_path_map.iter_forwards class_type t.class_types id;
      Forward_path_map.iter_forwards module_type t.module_types id;
      Forward_path_map.iter_forwards module_ t.modules id

    let find_type graph t typ =
      let canonical = Type.path graph typ in
      Forward_path_map.find t.types canonical

    let find_class_type graph t mty =
      let canonical = Class_type.path graph mty in
      Forward_path_map.find t.class_types canonical

    let find_module_type graph t mty =
      let canonical = Module_type.path graph mty in
      Forward_path_map.find t.module_types canonical

    let find_module graph t md =
      let canonical = Module.path graph md in
      Forward_path_map.find t.modules canonical

  end

  module Sections = struct

    type range =
      | Until of Height.t
      | All

    type versioning =
      | Unversioned
      | Initialisation of History.Stamp.t
      | Completion of History.Stamp.t

    type t =
      { mutable sections : Section.t Height.Array.t;
        mutable initialised : range;
        mutable completed : range;
        mutable versioning : versioning; }

    let create age origin =
      let sections = Height.Array.empty in
      let completed = Until Height.one in
      let initialised, versioning =
        if Age.equal age Age.zero then begin
          All, Completion History.Stamp.zero
        end else begin
          match origin with
          | Origin.Environment age' ->
              let initialised =
                if Age.less_than_or_equal age age' then All
                else Until Height.one
              in
              initialised, Unversioned
          | Origin.Dependency _ | Origin.Dependencies _ ->
              Until Height.one, Initialisation History.Stamp.zero
        end
      in
      { sections; initialised; completed; versioning; }

    let update t stamp =
      match t.versioning with
      | Unversioned -> ()
      | Initialisation initialised ->
          if History.Stamp.less_than initialised stamp then begin
            t.initialised <- Until Height.one;
            t.versioning <- Initialisation stamp
          end
      | Completion completed ->
          if History.Stamp.less_than completed stamp then begin
            t.completed <- Until Height.one;
            t.versioning <- Completion stamp
          end

    let expand t height =
      let sections = t.sections in
      if not (Height.Array.contains sections height) then begin
        let sections =
          Height.Array.extend sections height
            (fun _ -> Section.create ())
        in
        t.sections <- sections;
        sections
      end else begin
        sections
      end

    let is_initialised t height =
      match t.initialised with
      | All -> true
      | Until until -> Height.less_than height until

    let set_initialised t height =
      match t.initialised with
      | All ->
          failwith "Section.set_initialised: already initialised"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_initialised: initialised early"
            else
              failwith "Section.set_initialised: already initialised"
          end;
          t.initialised <- Until (Height.succ until)

    let set_initialised_from t height =
      match t.initialised with
      | All ->
          failwith "Section.set_initialised: already initialised"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_initialised: initialised early"
            else
              failwith "Section.set_initialised: already initialised"
          end;
          t.initialised <- All

    let is_completed t height =
      match t.completed with
      | All -> true
      | Until until -> Height.less_than height until

    let set_completed t height =
      match t.completed with
      | All ->
          failwith "Section.set_completed: already completed"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_completed: completed early"
            else
              failwith "Section.set_completed: already completed"
          end;
          t.completed <- Until (Height.succ until)

    let set_completed_from t height =
      match t.completed with
      | All ->
          failwith "Section.set_completed: already completed"
      | Until until ->
          if not (Height.equal until height) then begin
            if Height.less_than until height then
              failwith "Section.set_completed: completed early"
            else
              failwith "Section.set_completed: already completed"
          end;
          t.completed <- All

    let is_finished t =
      match t.initialised, t.completed with
      | All, All -> true
      | _, _ -> false

    let get t height =
      let sections = t.sections in
      if Height.Array.contains sections height then
        Some (Height.Array.get sections height)
      else None

    let check_initialised t height =
      match t.initialised with
      | All -> ()
      | Until until ->
          if not (Height.less_than height until) then
            failwith "Sections: section not initialised"

    let check_completed t height =
      match t.completed with
      | All -> ()
      | Until until ->
          if not (Height.less_than height until) then
            failwith "Sections: section not completed"

    let check_versions t parent =
      match t.versioning, parent.versioning with
      | Unversioned, _ | _, Unversioned -> ()
      | (Completion stamp | Initialisation stamp),
        (Completion parent_stamp | Initialisation parent_stamp) ->
          if not (History.Stamp.equal stamp parent_stamp) then
            failwith "Sections: version mismatch"

    let initialise t height parent =
      check_versions t parent;
      check_completed parent height;
      match get parent height with
      | Some parent ->
          let sections = expand t height in
          let section = Height.Array.get sections height in
          Section.rebase section parent;
          set_initialised t height
      | None ->
          if is_finished parent then
            set_initialised_from t height
          else
            set_initialised t height

    let add_type graph t height typ path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_type graph section typ path

    let add_class_type graph t height mty path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_class_type graph section mty path

    let add_module_type graph t height mty path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_module_type graph section mty path

    let add_module graph t height md path =
      let sections = expand t height in
      let section = Height.Array.get sections height in
      Section.add_module graph section md path

    (* returns [true] if there might be updated paths at a greater height. *)
    let iter_updates ~type_ ~class_type ~module_type ~module_ t height id =
      match get t height with
      | Some section ->
          Section.iter_updates ~type_ ~class_type
            ~module_type ~module_ section id;
          true
      | None -> false

    (* returns [true] if there might be forward paths at a greater height. *)
    let iter_forwards ~type_ ~class_type ~module_type ~module_ t height id =
      let all_initialised =
        match t.initialised with
        | All -> true
        | Until until ->
            if not (Height.less_than height until) then
              failwith "Sections.iter_forwards: section not initialised";
            false
      in
      match get t height with
      | Some section ->
          Section.iter_forwards ~type_ ~class_type
            ~module_type ~module_ section id;
          true
      | None -> not all_initialised

    type result =
      | Not_found_here
      | Not_found_here_or_later
      | Found of Path.t

    let rec get_visible_type graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_type_path_visible graph path in
          if visible then Some path
          else get_visible_type graph rest

    let rec get_visible_class_type graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_class_type_path_visible graph path in
          if visible then Some path
          else get_visible_class_type graph rest

    let rec get_visible_module_type graph = function
      | [] -> None
      | path :: rest ->
          let visible = Graph.is_module_type_path_visible graph path in
          if visible then Some path
          else get_visible_module_type graph rest

    let rec get_visible_module graph = function
      | [] -> None
      | (path, _) :: rest ->
          let visible = Graph.is_module_path_visible graph path in
          if visible then Some path
          else get_visible_module graph rest

    let find_type graph t height typ =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_type graph section typ with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_type graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

    let find_class_type graph t height mty =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_class_type graph section mty with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_class_type graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

    let find_module_type graph t height mty =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_module_type graph section mty with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_module_type graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

    let find_module graph t height md =
      check_initialised t height;
      check_completed t height;
      match get t height with
      | Some section -> begin
          match Section.find_module graph section md with
          | exception Not_found -> Not_found_here
          | paths -> begin
              match get_visible_module graph paths with
              | None -> Not_found_here
              | Some path -> Found path
            end
        end
      | None ->
          if is_finished t then Not_found_here_or_later
          else Not_found_here

  end

  type basis

  type env

  type _ kind =
    | Basis :
        { history : History.t; }
      -> basis kind
    | Env :
        { mutable revision : History.Revision.t;
          parent : 'a t;
          age : Age.t; }
      -> env kind

  and 'a t =
    { kind : 'a kind;
      mutable graph : Graph.t;
      sections: Sections.t Origin_tbl.t;
      todos: Todo.t; }

  let age (type k) (t : k t) =
    match t.kind with
    | Basis _ -> Age.zero
    | Env { age; _ } -> age

  let revision (type k) (t : k t) =
    match t.kind with
    | Basis { history } -> History.head history
    | Env { revision; _ } -> revision

  let stamp t =
    History.Revision.stamp (revision t)

  let rev_deps t =
    History.Revision.rev_deps (revision t)

  let update (type kind) (t : kind t) =
    match t.kind with
    | Basis _ -> ()
    | Env ({ revision } as e) ->
        let rec loop graph revision =
          let next = History.Revision.next revision in
          match next with
          | None -> revision, graph
          | Some revision ->
              let diff = History.Revision.diff revision in
              let graph = Graph.merge graph diff in
              let rev_deps = History.Revision.rev_deps revision in
              Todo.merge graph rev_deps t.todos diff;
              loop graph revision
        in
        let revision, graph = loop t.graph revision in
        t.graph <- graph;
        e.revision <- revision

  let basis rev_deps components =
    let graph, diff = Graph.add Graph.empty components in
    let history = History.init rev_deps diff in
    let kind = Basis { history } in
    let sections = Origin_tbl.create 0 in
    let todos = Todo.create graph rev_deps diff in
    { kind; graph; sections; todos }

  let local_or_open conc =
    match conc with
    | Desc.Local -> Component.Local
    | Desc.Open -> Component.Open

  let env parent desc =
    update parent;
    let age = Age.succ (age parent) in
    let origin = Origin.Environment age in
    let components =
      List.map
        (fun desc ->
           match desc with
           | Desc.Type(id, desc, conc, dpr) ->
               Component.Type(origin, id, desc, local_or_open conc, dpr)
           | Desc.Class_type(id, desc, conc, dpr) ->
               Component.Class_type(origin, id, desc, local_or_open conc, dpr)
           | Desc.Module_type(id, desc, conc, dpr) ->
               Component.Module_type(origin, id, desc, local_or_open conc, dpr)
           | Desc.Module(id, desc, conc, dpr) ->
               Component.Module(origin, id, desc, local_or_open conc, dpr)
           | Desc.Declare_type id ->
               Component.Declare_type(origin, id)
           | Desc.Declare_class_type id ->
               Component.Declare_class_type(origin, id)
           | Desc.Declare_module_type id ->
               Component.Declare_module_type(origin, id)
           | Desc.Declare_module id ->
               Component.Declare_module(origin, id))
        desc
    in
    let graph, diff = Graph.add parent.graph components in
    let revision = revision parent in
    let kind = Env { revision; parent; age } in
    let sections = Origin_tbl.create 0 in
    let rev_deps = History.Revision.rev_deps revision in
    let todos = Todo.create graph rev_deps diff in
    { kind; graph; sections; todos }

  let mutate (t : basis t) rev_deps components =
    let graph, diff = Graph.add t.graph components in
    let Basis { history } = t.kind in
    History.commit history rev_deps diff;
    t.graph <- graph;
    Todo.mutate graph rev_deps t.todos diff

  let sections t origin =
    match Origin_tbl.find t.sections origin with
    | exception Not_found ->
        let sections = Sections.create (age t) origin in
        Origin_tbl.add t.sections origin sections;
        sections
    | sections -> sections

  let update_seen t seen =
    Path_set.fold
      (fun path acc ->
         match acc with
         | None -> None
         | Some acc ->
             let md = Graph.find_module t.graph path in
             let path = Module.path t.graph md in
             if Path_set.mem path acc then None
             else Some (Path_set.add path acc))
      seen (Some Path_set.empty)

  let process_type t height path typ =
    let canonical_path = Type.path t.graph typ in
    if not (Path.equal canonical_path path) then begin
      let origin = Type.origin t.graph typ in
      let sections = sections t origin in
      Sections.add_type t.graph sections height typ path
    end

  let process_module_type t height path mty =
    let canonical_path = Module_type.path t.graph mty in
    if not (Path.equal canonical_path path) then begin
      let origin = Module_type.origin t.graph mty in
      let sections = sections t origin in
      Sections.add_module_type t.graph sections height mty path
    end

  let process_class_type t height path mty =
    let canonical_path = Class_type.path t.graph mty in
    if not (Path.equal canonical_path path) then begin
      let origin = Class_type.origin t.graph mty in
      let sections = sections t origin in
      Sections.add_class_type t.graph sections height mty path
    end

  let process_module t height path seen md =
    let canonical_path = Module.path t.graph md in
    if not (Path.equal canonical_path path) then begin
      let origin = Module.origin t.graph md in
      let sections = sections t origin in
      Sections.add_module t.graph sections height md (path, seen);
    end;
    if not (Path_set.mem canonical_path seen) then begin
      let seen = Path_set.add canonical_path seen in
      Todo.add_children t.graph (rev_deps t) t.todos height md path seen
    end

  let process_children t height path seen md =
    let types =
      match Module.types t.graph md with
      | Some types -> types
      | None -> String_map.empty
    in
    let class_types =
      match Module.class_types t.graph md with
      | Some class_types -> class_types
      | None -> String_map.empty
    in
    let module_types =
      match Module.module_types t.graph md with
      | Some module_types -> module_types
      | None -> String_map.empty
    in
    let modules =
      match Module.modules t.graph md with
      | Some modules -> modules
      | None -> String_map.empty
    in
    String_map.iter
      (fun name typ ->
         if not (Type.hidden typ) then begin
           let path = Path.Pdot(path, name) in
           process_type t height path typ
         end)
      types;
    String_map.iter
      (fun name clty ->
         if not (Class_type.hidden clty) then begin
           let path = Path.Pdot(path, name) in
           process_class_type t height path clty
         end)
      class_types;
    String_map.iter
      (fun name mty ->
         if not (Module_type.hidden mty) then begin
           let path = Path.Pdot(path, name) in
           process_module_type t height path mty
         end)
      module_types;
    String_map.iter
      (fun name md ->
         if not (Module.hidden md) then begin
           let path = Path.Pdot(path, name) in
           process_module t height path seen md
         end)
      modules

  let rec process : 'k . 'k t -> _ =
    fun t origin height ->
      let todo = Todo.pop (rev_deps t) t.todos height origin in
      match todo with
      | None -> true
      | Some items ->
          List.iter
            (function
              | Todo.Item.Base (Diff.Item.Type(id, typ, _)) ->
                  if not (Type.hidden typ) then begin
                    let path = Path.Pident id in
                    process_type t height path typ
                  end
              | Todo.Item.Base (Diff.Item.Class_type(id, clty, _)) ->
                  if not (Class_type.hidden clty) then begin
                    let path = Path.Pident id in
                    process_class_type t height path clty
                  end
              | Todo.Item.Base (Diff.Item.Module_type(id, mty, _)) ->
                  if not (Module_type.hidden mty) then begin
                    let path = Path.Pident id in
                    process_module_type t height path mty
                  end
              | Todo.Item.Base (Diff.Item.Module(id, md, _)) ->
                  if not (Module.hidden md) then begin
                    let path = Path.Pident id in
                    process_module t height path Path_set.empty md
                  end
              | Todo.Item.Children{md; path; seen} ->
                  process_children t height path seen md
              | Todo.Item.Update{ id; origin } ->
                  process_update t origin height id
              | Todo.Item.Forward{ id; decl; origin } ->
                  process_forward t origin height id decl)
            items;
            false

  and process_update : 'k . 'k t -> _ =
    fun t origin height id ->
      let sections = sections t origin in
      let more =
        Sections.iter_updates sections height id
          ~type_:(fun canon path ->
            let typ = Graph.find_type t.graph canon in
            process_type t height path typ)
          ~class_type:(fun canon path ->
            let clty = Graph.find_class_type t.graph canon in
            process_class_type t height path clty)
          ~module_type:(fun canon path ->
            let mty = Graph.find_module_type t.graph canon in
            process_module_type t height path mty)
          ~module_:(fun canon (path, seen) ->
            let md = Graph.find_module t.graph canon in
            match update_seen t seen with
            | None -> ()
            | Some seen ->
              process_module t height path seen md);
      in
      if more then begin
        Todo.add_next_update (rev_deps t) t.todos height origin id
      end


  and process_forward : 'k . 'k t -> _ =
    fun t origin height id decl ->
      let sections = init t decl height in
      let more =
        Sections.iter_forwards sections height id
          ~type_:(fun canon path ->
            let typ = Graph.find_type t.graph canon in
            process_type t height path typ)
          ~class_type:(fun canon path ->
            let clty = Graph.find_class_type t.graph canon in
            process_class_type t height path clty)
          ~module_type:(fun canon path ->
            let mty = Graph.find_module_type t.graph canon in
            process_module_type t height path mty)
          ~module_:(fun canon (path, seen) ->
            let md = Graph.find_module t.graph canon in
            match update_seen t seen with
            | None -> ()
            | Some seen ->
              process_module t height path seen md);
      in
      if more then begin
        Todo.add_next_forward (rev_deps t) t.todos height origin id decl
      end

  and initialise : type k. k t -> _ =
    fun t sections origin height ->
      if not (Sections.is_initialised sections height) then begin
        begin match Height.pred height with
        | None -> ()
        | Some pred -> initialise t sections origin pred
        end;
        let parent =
          match t.kind with
          | Basis _ -> assert false
          | Env { parent; _ } ->
              update parent;
              force parent origin height
        in
        Sections.initialise sections height parent
      end

  and init : 'k . 'k t -> _ =
    fun t origin height ->
      let sections = sections t origin in
      Sections.update sections (stamp t);
      initialise t sections origin height;
      sections

  and complete : 'k. 'k t -> _ =
    fun t sections origin height ->
      if not (Sections.is_completed sections height) then begin
        begin match Height.pred height with
        | None -> ()
        | Some pred -> ignore (complete t sections origin pred)
        end;
        let finished = process t origin height in
        if finished then Sections.set_completed_from sections height
        else Sections.set_completed sections height
      end

  and force : 'k. 'k t -> _ =
    fun t origin height ->
      let sections = sections t origin in
      Sections.update sections (stamp t);
      initialise t sections origin height;
      complete t sections origin height;
      sections

  module Search = struct

    type 'a shortest = 'a t

    type _ kind =
      | Type : Type.t kind
      | Class_type : Class_type.t kind
      | Module_type : Module_type.t kind
      | Module : Module.t kind

    type name =
      { name : string;
        height : Height.t; }

    type 'a t =
      | Ident of
          { kind : 'a kind;
            node : 'a;
            origin : Origin.t;
            best : Path.t;
            min: Height.t;
            max: Height.t;
            finished : bool; }
      | Dot of
          { kind : 'a kind;
            node : 'a;
            origin : Origin.t;
            best : Path.t;
            min: Height.t;
            max: Height.t;
            parent : Module.t t;
            name : name;
            searched : bool;
            finished : bool; }
      | Application of
          { kind : 'a kind;
            node : 'a;
            origin : Origin.t;
            best : Path.t;
            min: Height.t;
            max: Height.t;
            func : Module.t t;
            arg : Module.t t;
            func_first : bool;
            searched : bool;
            finished : bool; }

    let min_height = function
      | Ident { min; _ } -> min
      | Dot { min; _ } -> min
      | Application { min; _ } -> min

    let max_height = function
      | Ident { max; _ } -> max
      | Dot { max; _ } -> max
      | Application { max; _ } -> max

    let search_origin = function
      | Ident { origin; _ } -> origin
      | Dot { origin; _ } -> origin
      | Application { origin; _ } -> origin

    let finished = function
      | Ident { finished; _ } -> finished
      | Dot { finished; _ } -> finished
      | Application { finished; _ } -> finished

    let best = function
      | Ident { best; _ } -> best
      | Dot { best; _ } -> best
      | Application { best; _ } -> best

    let min_application fst snd =
      Height.plus (min_height fst) (min_height snd)

    let max_application fst snd =
      Height.plus (max_height fst) (max_height snd)

    let min_dot parent name =
      let base = min_height parent in
      Height.plus base name.height

    let path_application fst snd =
      Path.Papply(best fst, best snd)

    let path_dot parent name =
      Path.Pdot(best parent, name.name)

    let is_visible_ident (type k) graph (kind : k kind) id =
      match kind with
      | Type -> Graph.is_type_ident_visible graph id
      | Class_type -> Graph.is_class_type_ident_visible graph id
      | Module_type -> Graph.is_module_type_ident_visible graph id
      | Module -> Graph.is_module_ident_visible graph id

    let create (type k) shortest (kind : k kind) canonical_path =
      let rec loop : type k. k kind -> Path.t -> k t =
        fun kind path ->
          let graph = shortest.graph in
          let (node : k), origin, hidden =
            match kind with
            | Type ->
                let node = Graph.find_type graph path in
                let origin = Type.origin graph node in
                let hidden = Type.hidden node in
                node, origin, hidden
            | Class_type ->
                let node = Graph.find_class_type graph path in
                let origin = Class_type.origin graph node in
                let hidden = Class_type.hidden node in
                node, origin, hidden
            | Module_type ->
                let node = Graph.find_module_type graph path in
                let origin = Module_type.origin graph node in
                let hidden = Module_type.hidden node in
                node, origin, hidden
            | Module ->
                let node = Graph.find_module graph path in
                let origin = Module.origin graph node in
                let hidden = Module.hidden node in
                node, origin, hidden
          in
          let best = path in
          match path with
          | Path.Pident id ->
              let max =
                if is_visible_ident graph kind id && not hidden then
                  Height.one
                else
                  Height.maximum
              in
              let min = Height.one in
              let finished = false in
              Ident { kind; node; origin; best; min; max; finished }
          | Path.Pdot(parent, name) ->
              let parent = loop Module parent in
              let finished = false in
              let name_height =
                if not hidden then Height.one
                else Height.maximum
              in
              let name = { name; height = name_height } in
              let searched = false in
              let max = Height.plus (max_height parent) name_height in
              let min = Height.one in
              Dot
                { kind; node; origin; best; min; max;
                  parent; name; searched; finished }
          | Path.Papply(func, arg) ->
              let func = loop Module func in
              let arg = loop Module arg in
              let func_first =
                Rev_deps.before (rev_deps shortest)
                  (search_origin arg) (search_origin func)
              in
              let finished = false in
              (* There are no module aliases containing extended paths *)
              let searched = true in
              let max = max_application func arg in
              let min = min_application func arg in
              Application
                { kind; node; origin; best; min; max;
                  func; arg; func_first; searched; finished }
      in
      loop kind canonical_path

    let find (type k) shortest origin height (kind : k kind) (node : k) =
      let sections = force shortest origin height in
      match kind with
      | Type ->
          Sections.find_type shortest.graph sections height node
      | Class_type ->
          Sections.find_class_type shortest.graph sections height node
      | Module_type ->
          Sections.find_module_type shortest.graph sections height node
      | Module ->
          Sections.find_module shortest.graph sections height node

    let rec step : type k . _ shortest -> k t -> k t =
      fun shortest search ->
        if finished search then search
        else begin
          match search with
          | Ident r -> begin
              match find shortest r.origin r.min r.kind r.node with
              | Sections.Not_found_here ->
                  if Height.equal r.min r.max then
                    Ident { r with finished = true }
                  else
                    Ident { r with min = Height.succ r.min }
              | Sections.Not_found_here_or_later ->
                  Ident { r with finished = true; min = r.max }
              | Sections.Found path ->
                  let best = path in
                  let max = r.min in
                  let finished = true in
                  Ident { r with best; max; finished }
            end
          | Dot r ->
              let parent = r.parent in
              let parent =
                let should_try_dot =
                  Height.equal
                    (min_dot parent r.name) r.min
                in
                if not should_try_dot then parent
                else step shortest parent
              in
              let found =
                finished parent
                && Height.equal (min_dot parent r.name) r.min
              in
              if found then begin
                let best = path_dot parent r.name in
                let max = r.min in
                let finished = true in
                Dot
                  { r with best; parent; max; finished }
              end else begin
                let best, max, searched, finished =
                  if r.searched then r.best, r.max, r.searched, r.finished
                  else begin
                    match find shortest r.origin r.min r.kind r.node with
                    | Sections.Not_found_here ->
                        r.best, r.max, (Height.equal r.min r.max), r.finished
                    | Sections.Not_found_here_or_later ->
                        r.best, r.max, true, r.finished
                    | Sections.Found path ->
                        path, r.min, true, true
                  end
                in
                let finished =
                  finished ||
                  (searched
                   && Height.less_than_or_equal
                        r.max (min_dot parent r.name))
                in
                let min = if finished then max else Height.succ r.min in
                Dot { r with best; parent; min; max; searched; finished }
              end
         | Application r ->
              let try_app searched =
                let fst, snd =
                  if r.func_first then r.func, r.arg
                  else r.arg, r.func
                in
                let fst, snd =
                  let should_try_app =
                    Height.equal (min_application fst snd) r.min
                  in
                  if not should_try_app then fst, snd
                  else begin
                    let fst = step shortest fst in
                    let should_try_app =
                      Height.equal (min_application fst snd) r.min
                    in
                    if not should_try_app then fst, snd
                    else fst, step shortest snd
                  end
                in
                let func, arg =
                  if r.func_first then fst, snd
                  else snd, fst
                in
                let found =
                  finished func && finished arg
                  && Height.equal (min_application fst snd) r.min
                in
                if found then begin
                  let best = path_application func arg in
                  let max = r.min in
                  let finished = true in
                  Application
                    { r with best; func; arg; max; searched; finished }
                end else begin
                  let finished =
                    searched
                    && Height.less_than_or_equal
                         r.max (min_application fst snd)
                  in
                  let min = if finished then r.max else Height.succ r.min in
                  Application
                    { r with func; arg; min; searched; finished }
                end
              in
              if r.searched then try_app true
              else begin
                match find shortest r.origin r.min r.kind r.node with
                | Sections.Not_found_here ->
                    try_app (Height.equal r.min r.max)
                | Sections.Not_found_here_or_later ->
                    try_app true
                | Sections.Found path ->
                    let best = path in
                    let max = r.min in
                    let searched = true in
                    let finished = true in
                    Application { r with best; max; searched; finished }
              end
        end

    let rec perform shortest search =
      if finished search then best search
      else perform shortest (step shortest search)

  end

  let find_type t path =
    update t;
    let typ = Graph.find_type t.graph path in
    match Type.resolve t.graph typ with
    | Type.Nth n -> Nth n
    | Type.Path(subst, typ) ->
      let canonical_path = Type.path t.graph typ in
      let search = Search.create t Search.Type canonical_path in
      let path = Search.perform t search in
      Path(subst, path)

  let find_type_resolution t path : type_resolution =
    update t;
    let typ = Graph.find_type t.graph path in
    match Type.resolve t.graph typ with
    | Type.Nth n -> Nth n
    | Type.Path(Some ns, _) -> Subst ns
    | Type.Path(None, _) -> Id

  let find_type_simple t path =
    update t;
    let typ = Graph.find_type t.graph path in
    let canonical_path = Type.path t.graph typ in
    let search = Search.create t Search.Type canonical_path in
    Search.perform t search

  let find_class_type t path =
    update t;
    let clty = Graph.find_class_type t.graph path in
    let subst, clty = Class_type.resolve t.graph clty in
    let canonical_path = Class_type.path t.graph clty in
    let search = Search.create t Search.Class_type canonical_path in
    let path = Search.perform t search in
    (subst, path)

  let find_class_type_simple t path =
    update t;
    let clty = Graph.find_class_type t.graph path in
    let canonical_path = Class_type.path t.graph clty in
    let search = Search.create t Search.Class_type canonical_path in
    Search.perform t search

  let find_module_type t path =
    update t;
    let mty = Graph.find_module_type t.graph path in
    let canonical_path = Module_type.path t.graph mty in
    let search = Search.create t Search.Module_type canonical_path in
    Search.perform t search

  let find_module t path =
    update t;
    let md = Graph.find_module t.graph path in
    let canonical_path = Module.path t.graph md in
    let search = Search.create t Search.Module canonical_path in
    Search.perform t search

end

module String_set = Set.Make(String)

module Basis = struct

  type load =
    { name : string;
      depends : string list;
      alias_depends : string list;
      desc : Desc.Module.t;
      deprecated : Desc.deprecated; }

  type t =
    { mutable next_dep : Dependency.t;
      mutable pending_additions : String_set.t;
      mutable pending_loads : load list;
      mutable assignment : Dependency.t String_map.t;
      rev_deps : Rev_deps.t;
      mutable shortest : Shortest.basis Shortest.t option; }

  let create () =
    { next_dep = Dependency.zero;
      pending_additions = String_set.empty;
      pending_loads = [];
      assignment = String_map.empty;
      rev_deps = Rev_deps.create ();
      shortest = None; }

  let update_assignments t additions =
    String_set.iter
      (fun name ->
         if not (String_map.mem name t.assignment) then begin
           t.assignment <- String_map.add name t.next_dep t.assignment;
           t.next_dep <- Dependency.succ t.next_dep
         end)
      additions

  let update_rev_deps t loads =
    Rev_deps.extend_up_to t.rev_deps t.next_dep;
    List.iter
      (fun { name; depends; alias_depends; _ } ->
         let index = String_map.find name t.assignment in
         List.iter
           (fun dep_name ->
              let dep_index = String_map.find dep_name t.assignment in
              Rev_deps.add t.rev_deps ~source:dep_index ~target:index)
           depends;
         List.iter
           (fun dep_name ->
              let dep_index = String_map.find dep_name t.assignment in
              Rev_deps.add_alias t.rev_deps ~source:dep_index ~target:index)
           alias_depends)
      loads

  let update_shortest t additions loads =
    let components =
      List.map
        (fun { name; desc; deprecated; _ } ->
           let index = String_map.find name t.assignment in
           let origin = Origin.Dependency index in
           let id = Ident.global name in
           Component.Module(origin, id, desc, Component.Global, deprecated))
        loads
    in
    let components =
      String_set.fold
        (fun name acc ->
           let index = String_map.find name t.assignment in
           let origin = Origin.Dependency index in
           let id = Ident.global name in
           Component.Declare_module(origin, id) :: acc)
        additions
        components
    in
    match t.shortest with
    | None ->
        t.shortest <- Some (Shortest.basis t.rev_deps components)
    | Some shortest ->
        Shortest.mutate shortest t.rev_deps components

  let update t =
    let loads = t.pending_loads in
    let additions = t.pending_additions in
    match loads, String_set.is_empty additions with
    | [], true -> ()
    | _, _ ->
      t.pending_loads <- [];
      t.pending_additions <- String_set.empty;
      let loads = List.rev loads in
      update_assignments t additions;
      update_rev_deps t loads;
      update_shortest t additions loads

  let shortest t =
    update t;
    match t.shortest with
    | None ->
        let shortest = Shortest.basis t.rev_deps [] in
        t.shortest <- Some shortest;
        shortest
    | Some shortest -> shortest

  let add t name =
    t.pending_additions <- String_set.add name t.pending_additions

  let load t name depends alias_depends desc deprecated =
    let load = { name; depends; alias_depends; desc; deprecated } in
    t.pending_loads <- load :: t.pending_loads

end

type state =
  | Initial of Basis.t
  | Unforced of
      { parent : t;
        desc : Desc.t list Lazy.t; }
  | Forced of
      { basis : Basis.t;
        shortest : Shortest.env Shortest.t; }

and t = state ref

let rec force t =
  match !t with
  | Initial _ | Forced _ as state -> state
  | Unforced { parent; desc } ->
    let desc = Lazy.force desc in
    let state =
      match force parent with
      | Unforced _ -> assert false
      | Initial basis ->
        let shortest = Shortest.env (Basis.shortest basis) desc in
        Forced { basis; shortest }
      | Forced { basis; shortest } ->
        let shortest = Shortest.env shortest desc in
        Forced { basis; shortest }
    in
    t := state;
    state

let initial basis = ref (Initial basis)

let add parent desc =
  ref (Unforced { parent; desc })

type ext_shortest = Shortest : 'k Shortest.t -> ext_shortest

let shortest t =
  match force t with
  | Unforced _ -> assert false
  | Initial basis ->
      Basis.update basis;
      Shortest (Basis.shortest basis)
  | Forced { basis; shortest } ->
      Basis.update basis;
      Shortest shortest

let find_type t path =
  let Shortest shortest = shortest t in
  match Shortest.find_type shortest path with
  | exception Not_found -> Path(None, path)
  | result -> result

let find_type_resolution t path : type_resolution =
  let Shortest shortest = shortest t in
  match Shortest.find_type_resolution shortest path with
  | exception Not_found -> Id
  | subst -> subst

let find_type_simple t path =
  let Shortest shortest = shortest t in
  match Shortest.find_type_simple shortest path with
  | exception Not_found -> path
  | path -> path

let find_class_type t path =
  let Shortest shortest = shortest t in
  match Shortest.find_class_type shortest path with
  | exception Not_found -> (None, path)
  | result -> result

let find_class_type_simple t path =
  let Shortest shortest = shortest t in
  match Shortest.find_class_type_simple shortest path with
  | exception Not_found -> path
  | path -> path

let find_module_type t path =
  let Shortest shortest = shortest t in
  match Shortest.find_module_type shortest path with
  | exception Not_found -> path
  | path -> path

let find_module t path =
  let Shortest shortest = shortest t in
  match Shortest.find_module shortest path with
  | exception Not_found -> path
  | path -> path
