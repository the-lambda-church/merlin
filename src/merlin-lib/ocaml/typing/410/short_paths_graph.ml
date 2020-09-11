
module String_map = Misc.String.Map

module Ident = struct

  type t = Ident.t

  let equal t1 t2 = Ident.equal t1 t2

  let compare t1 t2 = Ident.compare t1 t2

  let name = Ident.name

  let global name =
    Ident.create_persistent name

end

module Ident_map = Map.Make(Ident)
module Ident_set = Set.Make(Ident)

module Path = struct

  type t = Path.t =
    | Pident of Ident.t
    | Pdot of t * string
    | Papply of t * t

  open Path

  let rec equal t1 t2 =
    match t1, t2 with
    | Pident id1, Pident id2 -> Ident.equal id1 id2
    | Pident _, Pdot _ -> false
    | Pident _, Papply _ -> false
    | Pdot _, Pident _ -> false
    | Pdot(parent1, name1), Pdot(parent2, name2) ->
        equal parent1 parent2
        && String.equal name1 name2
    | Pdot _, Papply _ -> false
    | Papply _, Pident _ -> false
    | Papply _, Pdot _ -> false
    | Papply(func1, arg1), Papply(func2, arg2) ->
        equal func1 func2
        && equal arg1 arg2

  let rec compare t1 t2 =
    match t1, t2 with
    | Pident id1, Pident id2 -> Ident.compare id1 id2
    | Pident _, Pdot _ -> -1
    | Pident _, Papply _ -> -1
    | Pdot _, Pident _ -> 1
    | Pdot(parent1, name1), Pdot(parent2, name2) ->
        let c = compare parent1 parent2 in
        if c <> 0 then c
        else String.compare name1 name2
    | Pdot _, Papply _ -> -1
    | Papply _, Pident _ -> 1
    | Papply _, Pdot _ -> 1
    | Papply(func1, arg1), Papply(func2, arg2) ->
        let c = compare func1 func2 in
        if c <> 0 then c
        else compare arg1 arg2

end

module Path_map = Map.Make(Path)
module Path_set = Set.Make(Path)

module Desc = struct

  type deprecated =
    | Deprecated
    | Not_deprecated

  module Type = struct

    type t =
      | Fresh
      | Nth of int
      | Subst of Path.t * int list
      | Alias of Path.t

  end

  module Module_type = struct

    type t =
      | Fresh
      | Alias of Path.t

  end

  module Class_type = struct

    type t =
      | Fresh
      | Subst of Path.t * int list
      | Alias of Path.t

  end

  module Module = struct

    type component =
      | Type of string * Type.t * deprecated
      | Class_type of string * Class_type.t * deprecated
      | Module_type of string * Module_type.t * deprecated
      | Module of string * t * deprecated

    and components = component list

    and kind =
      | Signature of components Lazy.t
      | Functor of (Path.t -> t)

    and t =
      | Fresh of kind
      | Alias of Path.t

  end

  type source =
    | Local
    | Open

  type t =
    | Type of Ident.t * Type.t * source * deprecated
    | Class_type of Ident.t * Class_type.t * source * deprecated
    | Module_type of Ident.t * Module_type.t * source * deprecated
    | Module of Ident.t * Module.t * source * deprecated
    | Declare_type of Ident.t
    | Declare_class_type of Ident.t
    | Declare_module_type of Ident.t
    | Declare_module of Ident.t

end

module Sort = struct

  type t =
    | Defined
    | Declared of Ident_set.t

  let application t1 t2 =
    match t1, t2 with
    | Defined, Defined -> Defined
    | Defined, Declared _ -> t2
    | Declared _, Defined -> t1
    | Declared ids1, Declared ids2 -> Declared (Ident_set.union ids1 ids2)

end

module Age = Natural.Make()

module Dependency = Natural.Make()

module Origin = struct

  type t =
    | Dependency of Dependency.t
    | Dependencies of Dependency.t list
    | Environment of Age.t

  let rec deps_add dep deps =
    match deps with
    | [] -> [dep]
    | dep' :: rest ->
      if Dependency.equal dep dep' then
        deps
      else if Dependency.less_than dep dep' then
        dep :: deps
      else
        dep' :: deps_add dep rest

  let rec deps_union deps1 deps2 =
    match deps1, deps2 with
    | [], _ -> deps2
    | _, [] -> deps1
    | dep1 :: rest1, dep2 :: rest2 ->
        if Dependency.equal dep1 dep2 then
          dep1 :: deps_union rest1 rest2
        else if Dependency.less_than dep1 dep2 then
          dep1 :: deps_union rest1 deps2
        else
          dep2 :: deps_union deps1 rest2

  let rec deps_equal deps1 deps2 =
    match deps1, deps2 with
    | [], [] -> true
    | [], _ :: _ -> false
    | _ :: _, [] -> false
    | dep1 :: rest1, dep2 :: rest2 ->
        Dependency.equal dep1 dep2
        && deps_equal rest1 rest2

  let application t1 t2 =
    match t1, t2 with
    | Dependency dep1, Dependency dep2 ->
        if Dependency.equal dep1 dep2 then t1
        else if Dependency.less_than dep1 dep2 then
          Dependencies [dep1; dep2]
        else
          Dependencies [dep2; dep1]
    | Dependency dep1, Dependencies deps2 ->
        Dependencies (deps_add dep1 deps2)
    | Dependency _, Environment _ -> t2
    | Dependencies deps1, Dependency dep2 ->
        Dependencies (deps_add dep2 deps1)
    | Dependencies deps1, Dependencies deps2 ->
        Dependencies (deps_union deps1 deps2)
    | Dependencies _, Environment _ -> t2
    | Environment _, Dependency _ -> t1
    | Environment _, Dependencies _ -> t1
    | Environment age1, Environment age2 ->
        Environment (Age.max age1 age2)

  let equal t1 t2 =
    match t1, t2 with
    | Dependency dep1, Dependency dep2 -> Dependency.equal dep1 dep2
    | Dependency _, Dependencies _ -> false
    | Dependency _, Environment _ -> false
    | Dependencies _, Dependency _ -> false
    | Dependencies deps1, Dependencies deps2 -> deps_equal deps1 deps2
    | Dependencies _, Environment _ -> false
    | Environment _, Dependency _ -> false
    | Environment _, Dependencies _ -> false
    | Environment env1, Environment env2 -> Age.equal env1 env2

  let hash = Hashtbl.hash

end

let hidden_name name =
  if name <> "" && name.[0] = '_' then true
  else
    try
      for i = 1 to String.length name - 2 do
        if name.[i] = '_' && name.[i + 1] = '_' then
          raise Exit
      done;
      false
    with Exit -> true

let hidden_ident id =
  if !Clflags.unsafe_string && Ident.equal id Predef.ident_bytes then true
  else hidden_name (Ident.name id)

let hidden_definition deprecated name =
  match deprecated with
  | Desc.Deprecated -> true
  | Desc.Not_deprecated -> hidden_name name

let hidden_base_definition deprecated id =
  match deprecated with
  | Desc.Deprecated -> true
  | Desc.Not_deprecated -> hidden_ident id

module rec Type : sig

  type t

  val base : Origin.t -> Ident.t -> Desc.Type.t option -> Desc.deprecated -> t

  val child :
    Module.normalized -> string -> Desc.Type.t option -> Desc.deprecated -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val hidden : t -> bool

  val sort : Graph.t -> t -> Sort.t

  type resolved =
    | Nth of int
    | Path of int list option * t

  val resolve : Graph.t -> t -> resolved

end = struct

  open Desc.Type

  type definition =
    | Alias of Path.t
    | Defined
    | Nth of int
    | Subst of Path.t * int list
    | Unknown

  type t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t;
          hidden : bool; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          hidden : bool;
          sort : Sort.t;
          definition : definition; }

  let definition_of_desc (desc : Desc.Type.t option) =
    match desc with
    | None -> Unknown
    | Some Fresh -> Defined
    | Some (Nth n) -> Nth n
    | Some (Subst(p, ns)) -> Subst(p, ns)
    | Some (Alias alias) -> Alias alias

  let base origin id desc deprecated =
    let path = Path.Pident id in
    let hidden = hidden_base_definition deprecated id in
    let sort = Sort.Defined in
    let definition = definition_of_desc desc in
    Definition { origin; path; hidden; sort; definition }

  let child md name desc deprecated =
    let origin = Module.raw_origin md in
    let sort = Module.raw_sort md in
    let path = Path.Pdot(Module.raw_path md, name) in
    let hidden = hidden_definition deprecated name in
    let definition = definition_of_desc desc in
    Definition { origin; path; hidden; sort; definition }

  let declare origin id =
    let hidden = hidden_ident id in
    Declaration { origin; id; hidden }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let hidden t =
    match t with
    | Definition { hidden; _ } -> hidden
    | Declaration { hidden; _ } -> hidden

  let raw_origin t =
    match t with
    | Declaration { origin; _ }
    | Definition { origin; _ } -> origin

  let raw_path t =
    match t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let raw_sort t =
    match t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  let rec normalize_loop root t =
    match t with
    | Declaration _ -> t
    | Definition { definition = Defined | Unknown | Nth _ | Subst _ } -> t
    | Definition ({ definition = Alias alias } as r) -> begin
        match Graph.find_type root alias with
        | exception Not_found -> Definition { r with definition = Unknown }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    match t with
    | Definition { sort = Sort.Defined } -> normalize_loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_type root (raw_path t) with
        | exception Not_found -> normalize_loop root t
        | t -> normalize_loop root t

  let origin root t =
    raw_origin (normalize root t)

  let path root t =
    raw_path (normalize root t)

  let sort root t =
    raw_sort (normalize root t)

  type resolved =
    | Nth of int
    | Path of int list option * t

  let subst ns = function
    | Nth n -> Nth (List.nth ns n)
    | Path(None, p) -> Path(Some ns, p)
    | Path(Some ms, p) -> Path(Some (List.map (List.nth ns) ms), p)

  let rec resolve root t =
    match normalize root t with
    | Declaration _ -> Path(None, t)
    | Definition { definition = Defined | Unknown } -> Path(None, t)
    | Definition { definition = Nth n } -> Nth n
    | Definition { definition = Subst(p, ns) } -> begin
        match Graph.find_type root p with
        | exception Not_found -> Path(None, t)
        | aliased -> subst ns (resolve root aliased)
      end
    | Definition { definition = Alias _ } -> assert false

end

and Class_type : sig

  type t

  val base :
    Origin.t -> Ident.t -> Desc.Class_type.t option -> Desc.deprecated -> t

  val child :
    Module.normalized -> string ->
    Desc.Class_type.t option -> Desc.deprecated -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val hidden : t -> bool

  val sort : Graph.t -> t -> Sort.t

  type resolved = int list option * t

  val resolve : Graph.t -> t -> resolved

end = struct

  open Desc.Class_type

  type definition =
    | Alias of Path.t
    | Defined
    | Subst of Path.t * int list
    | Unknown

  type t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t;
          hidden : bool; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          hidden : bool;
          sort : Sort.t;
          definition : definition; }

  let definition_of_desc (desc : Desc.Class_type.t option) =
    match desc with
    | None -> Unknown
    | Some Fresh -> Defined
    | Some (Subst(p, ns)) -> Subst(p, ns)
    | Some (Alias alias) -> Alias alias

  let base origin id desc deprecated =
    let path = Path.Pident id in
    let hidden = hidden_base_definition deprecated id in
    let sort = Sort.Defined in
    let definition = definition_of_desc desc in
    Definition { origin; path; hidden; sort; definition }

  let child md name desc deprecated =
    let origin = Module.raw_origin md in
    let sort = Module.raw_sort md in
    let path = Path.Pdot(Module.raw_path md, name) in
    let hidden = hidden_definition deprecated name in
    let definition = definition_of_desc desc in
    Definition { origin; path; hidden; sort; definition }

  let declare origin id =
    let hidden = hidden_ident id in
    Declaration { origin; id; hidden }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let hidden t =
    match t with
    | Definition { hidden; _ } -> hidden
    | Declaration { hidden; _ } -> hidden

  let raw_origin t =
    match t with
    | Declaration { origin; _ }
    | Definition { origin; _ } -> origin

  let raw_path t =
    match t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let raw_sort t =
    match t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  let rec normalize_loop root t =
    match t with
    | Declaration _ -> t
    | Definition { definition = Defined | Unknown | Subst _ } -> t
    | Definition ({ definition = Alias alias } as r) -> begin
        match Graph.find_class_type root alias with
        | exception Not_found -> Definition { r with definition = Unknown }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    match t with
    | Definition { sort = Sort.Defined } -> normalize_loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_class_type root (raw_path t) with
        | exception Not_found -> normalize_loop root t
        | t -> normalize_loop root t

  let origin root t =
    raw_origin (normalize root t)

  let path root t =
    raw_path (normalize root t)

  let sort root t =
    raw_sort (normalize root t)

  type resolved = int list option * t

  let subst ns = function
    | (None, p) -> (Some ns, p)
    | (Some ms, p) -> (Some (List.map (List.nth ns) ms), p)

  let rec resolve root t =
    match normalize root t with
    | Declaration _ -> (None, t)
    | Definition { definition = Defined | Unknown } -> (None, t)
    | Definition { definition = Subst(p, ns) } -> begin
        match Graph.find_class_type root p with
        | exception Not_found -> (None, t)
        | aliased -> subst ns (resolve root aliased)
      end
    | Definition { definition = Alias _ } -> assert false

end

and Module_type : sig

  type t

  val base :
    Origin.t -> Ident.t -> Desc.Module_type.t option -> Desc.deprecated -> t

  val child :
    Module.normalized -> string ->
    Desc.Module_type.t option -> Desc.deprecated -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val hidden : t -> bool

  val sort : Graph.t -> t -> Sort.t

end = struct

  open Desc.Module_type

  type definition =
    | Alias of Path.t
    | Defined
    | Unknown

  type t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t;
          hidden : bool; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          hidden : bool;
          sort : Sort.t;
          definition : definition; }

  let base origin id desc deprecated =
    let path = Path.Pident id in
    let hidden = hidden_base_definition deprecated id in
    let sort = Sort.Defined in
    let definition =
      match desc with
      | None -> Unknown
      | Some Fresh -> Defined
      | Some (Alias alias) -> Alias alias
    in
    Definition { origin; path; hidden; sort; definition }

  let child md name desc deprecated =
    let origin = Module.raw_origin md in
    let sort = Module.raw_sort md in
    let path = Path.Pdot (Module.raw_path md, name) in
    let hidden = hidden_definition deprecated name in
    let definition =
      match desc with
      | None -> Unknown
      | Some Fresh -> Defined
      | Some (Alias alias) -> Alias alias
    in
    Definition { origin; path; hidden; sort; definition }

  let declare origin id =
    let hidden = hidden_ident id in
    Declaration { origin; id; hidden }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let hidden t =
    match t with
    | Definition { hidden; _ } -> hidden
    | Declaration { hidden; _ } -> hidden

  let raw_origin t =
    match t with
    | Declaration { origin; _ } | Definition { origin; _ } ->
        origin

  let raw_path t =
    match t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let raw_sort t =
    match t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  let rec normalize_loop root t =
    match t with
    | Declaration _ -> t
    | Definition { definition = Defined | Unknown } -> t
    | Definition ({ definition = Alias alias } as r) -> begin
        match Graph.find_module_type root alias with
        | exception Not_found -> Definition { r with definition = Unknown }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    match t with
    | Definition { sort = Sort.Defined } -> normalize_loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_module_type root (raw_path t) with
        | exception Not_found -> normalize_loop root t
        | t -> normalize_loop root t

  let origin root t =
    raw_origin (normalize root t)

  let path root t =
    raw_path (normalize root t)

  let sort root t =
    raw_sort (normalize root t)

end

and Module : sig

  type t

  type normalized

  val base :
    Origin.t -> Ident.t -> Desc.Module.t option -> Desc.deprecated -> t

  val child :
    normalized -> string -> Desc.Module.t option -> Desc.deprecated -> t

  val application : normalized -> t -> Desc.Module.t option -> t

  val declare : Origin.t -> Ident.t -> t

  val declaration : t -> Origin.t option

  val origin : Graph.t -> t -> Origin.t

  val path : Graph.t -> t -> Path.t

  val hidden : t -> bool

  val sort : Graph.t -> t -> Sort.t

  val types : Graph.t -> t -> Type.t String_map.t option

  val class_types : Graph.t -> t -> Class_type.t String_map.t option

  val module_types : Graph.t -> t -> Module_type.t String_map.t option

  val modules : Graph.t -> t -> Module.t String_map.t option

  val find_type : Graph.t -> t -> string -> Type.t

  val find_class_type : Graph.t -> t -> string -> Class_type.t

  val find_module_type : Graph.t -> t -> string -> Module_type.t

  val find_module : Graph.t -> t -> string -> Module.t

  val find_application : Graph.t -> t -> Path.t -> Module.t

  val normalize : Graph.t -> t -> normalized

  val unnormalize : normalized -> t

  val raw_origin : normalized -> Origin.t

  val raw_path : normalized -> Path.t

  val raw_sort : normalized -> Sort.t

end = struct

  open Desc.Module

  type components =
    | Unforced of Desc.Module.components Lazy.t
    | Forced of
        { types : Type.t String_map.t;
          class_types : Class_type.t String_map.t;
          module_types : Module_type.t String_map.t;
          modules : t String_map.t; }

  and definition =
    | Alias of Path.t
    | Signature of
        { mutable components : components }
    | Functor of
        { apply : Path.t -> Desc.Module.t;
          mutable applications : t Path_map.t; }
    | Unknown

  and t =
    | Declaration of
        { origin : Origin.t;
          id : Ident.t;
          hidden : bool; }
    | Definition of
        { origin : Origin.t;
          path : Path.t;
          hidden : bool;
          sort : Sort.t;
          definition : definition; }

  let base origin id desc deprecated =
    let path = Path.Pident id in
    let hidden = hidden_base_definition deprecated id in
    let sort = Sort.Defined in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Alias alias
    in
    Definition { origin; path; hidden; sort; definition }

  let child md name desc deprecated =
    let origin = Module.raw_origin md in
    let sort = Module.raw_sort md in
    let path = Path.Pdot(Module.raw_path md, name) in
    let hidden = hidden_definition deprecated name in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Alias alias
    in
    Definition { origin; path; hidden; sort; definition }

  let application func arg desc =
    let func_origin = Module.raw_origin func in
    let arg_origin = Module.raw_origin arg in
    let origin = Origin.application func_origin arg_origin in
    let func_sort = Module.raw_sort func in
    let arg_sort = Module.raw_sort arg in
    let sort = Sort.application func_sort arg_sort in
    let func_path = Module.raw_path func in
    let arg_path = Module.raw_path arg in
    let path = Path.Papply(func_path, arg_path) in
    let hidden = false in
    let definition =
      match desc with
      | None -> Unknown
      | Some (Fresh (Signature components)) ->
          let components = Unforced components in
          Signature { components }
      | Some (Fresh (Functor apply)) ->
          let applications = Path_map.empty in
          Functor { apply; applications }
      | Some (Alias alias) ->
          Alias alias
    in
    Definition { origin; path; hidden; sort; definition }

  let declare origin id =
    let hidden = hidden_ident id in
    Declaration { origin; id; hidden }

  let declaration t =
    match t with
    | Definition _ -> None
    | Declaration { origin; _} -> Some origin

  let hidden t =
    match t with
    | Definition { hidden; _ } -> hidden
    | Declaration { hidden; _ } -> hidden

  let raw_origin t =
    match t with
    | Declaration { origin; _ } | Definition { origin; _ } ->
        origin

  let raw_path t =
    match t with
    | Declaration { id; _ } -> Path.Pident id
    | Definition { path; _ } -> path

  let raw_sort t =
    match t with
    | Declaration { id; _ } -> Sort.Declared (Ident_set.singleton id)
    | Definition { sort; _ } -> sort

  type normalized = t

  let rec normalize_loop root t =
    match t with
    | Declaration _ -> t
    | Definition { definition = Signature _ | Functor _ | Unknown } -> t
    | Definition ({ definition = Alias alias } as r) -> begin
        match Graph.find_module root alias with
        | exception Not_found -> Definition { r with definition = Unknown }
        | aliased -> normalize_loop root aliased
      end

  let normalize root t =
    match t with
    | Definition { sort = Sort.Defined } -> normalize_loop root t
    | Definition { sort = Sort.Declared _ } | Declaration _ ->
        match Graph.find_module root (raw_path t) with
        | exception Not_found -> normalize_loop root t
        | t -> normalize_loop root t

  let unnormalize t = t

  let origin root t =
    raw_origin (normalize root t)

  let path root t =
    raw_path (normalize root t)

  let sort root t =
    raw_sort (normalize root t)

  let definition t =
    match Module.unnormalize t with
    | Declaration _ -> Unknown
    | Definition { definition; _ } -> definition

  let force root t =
    let t = Module.normalize root t in
    match definition t with
    | Alias _ -> assert false
    | Unknown
    | Functor _
    | Signature { components = Forced _ } -> t
    | Signature ({ components = Unforced components; _} as r) -> begin
        let rec loop types class_types module_types modules = function
          | [] -> Forced { types; class_types; module_types; modules }
          | Type(name, desc, dpr) :: rest ->
              let typ = Type.child t name (Some desc) dpr in
              let types = String_map.add name typ types in
              loop types class_types module_types modules rest
          | Class_type(name, desc, dpr) :: rest ->
              let clty = Class_type.child t name (Some desc) dpr in
              let class_types = String_map.add name clty class_types in
              loop types class_types module_types modules rest
          | Module_type(name, desc, dpr) :: rest ->
              let mty = Module_type.child t name (Some desc) dpr in
              let module_types = String_map.add name mty module_types in
              loop types class_types module_types modules rest
          | Module(name, desc, dpr) :: rest ->
              let md = Module.child t name (Some desc) dpr in
              let modules = String_map.add name md modules in
              loop types class_types module_types modules rest
        in
        let empty = String_map.empty in
        let components = loop empty empty empty empty (Lazy.force components) in
        r.components <- components;
        t
      end

  let types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { types; _ }; _ } ->
        Some types

  let class_types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { class_types; _ } } ->
        Some class_types

  let module_types root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { module_types; _ } } ->
        Some module_types

  let modules root t =
    let t = force root t in
    match definition t with
    | Alias _ | Signature { components = Unforced _ } ->
        assert false
    | Unknown | Functor _ ->
        None
    | Signature { components = Forced { modules; _ } } ->
        Some modules

  let find_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Type.child t name None Not_deprecated
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { types; _ }; _ } ->
        String_map.find name types

  let find_class_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Class_type.child t name None Not_deprecated
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { class_types; _ }; _ } ->
        String_map.find name class_types

  let find_module_type root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Module_type.child t name None Not_deprecated
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { module_types; _ }; _ } ->
        String_map.find name module_types

  let find_module root t name =
    let t = force root t in
    match definition t with
    | Alias _
    | Signature { components = Unforced _ } ->
        assert false
    | Unknown ->
        Module.child t name None Not_deprecated
    | Functor _ ->
        raise Not_found
    | Signature { components = Forced { modules; _ }; _ } ->
        String_map.find name modules

  let find_application root t path =
    let t = Module.normalize root t in
    match definition t with
    | Alias _ -> assert false
    | Signature _ -> raise Not_found
    | Unknown ->
        let arg = Graph.find_module root path in
        Module.application t arg None
    | Functor ({ apply; applications } as r)->
        let arg = Graph.find_module root path in
        let arg_path = Module.path root arg in
        match Path_map.find arg_path applications with
        | md -> md
        | exception Not_found ->
            let md = Module.application t arg (Some (apply arg_path)) in
            r.applications <- Path_map.add arg_path md applications;
            md

end

and Diff : sig

  module Item : sig

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Class_type of Ident.t * Class_type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    val origin : Graph.t -> t -> Origin.t

    val id : Graph.t -> t -> Ident.t

    val previous : Graph.t -> t -> Origin.t option

  end

  type t = Item.t list

end = struct

  module Item = struct

    type t =
      | Type of Ident.t * Type.t * Origin.t option
      | Class_type of Ident.t * Class_type.t * Origin.t option
      | Module_type of Ident.t * Module_type.t * Origin.t option
      | Module of Ident.t * Module.t * Origin.t option

    let origin root = function
      | Type(_, typ, _) -> Type.origin root typ
      | Class_type(_, clty, _) -> Class_type.origin root clty
      | Module_type(_, mty, _) -> Module_type.origin root mty
      | Module(_, md, _) -> Module.origin root md

    let id _root = function
      | Type(id, _, _) -> id
      | Class_type(id, _, _) -> id
      | Module_type(id, _, _) -> id
      | Module(id, _, _) -> id

    let previous _root = function
      | Type(_, _, prev) -> prev
      | Class_type(_, _, prev) -> prev
      | Module_type(_, _, prev) -> prev
      | Module(_, _, prev) -> prev

  end

  type t = Item.t list

end

and Component : sig

  type source =
    | Global
    | Local
    | Open

  type t =
    | Type of
        Origin.t * Ident.t * Desc.Type.t * source * Desc.deprecated
    | Class_type of
        Origin.t * Ident.t * Desc.Class_type.t * source * Desc.deprecated
    | Module_type of
        Origin.t * Ident.t * Desc.Module_type.t * source * Desc.deprecated
    | Module of
        Origin.t * Ident.t * Desc.Module.t * source * Desc.deprecated
    | Declare_type of Origin.t * Ident.t
    | Declare_class_type of Origin.t * Ident.t
    | Declare_module_type of Origin.t * Ident.t
    | Declare_module of Origin.t * Ident.t

end = Component

and Graph : sig

  type t

  val empty : t

  val add : t -> Component.t list -> t * Diff.t

  val merge : t -> Diff.t -> t

  val find_type : t -> Path.t -> Type.t

  val find_class_type : t -> Path.t -> Class_type.t

  val find_module_type : t -> Path.t -> Module_type.t

  val find_module : t -> Path.t -> Module.t

  val is_type_path_visible : t -> Path.t -> bool

  val is_class_type_path_visible : t -> Path.t -> bool

  val is_module_type_path_visible : t -> Path.t -> bool

  val is_module_path_visible : t -> Path.t -> bool

  val is_type_ident_visible : t -> Ident.t -> bool

  val is_class_type_ident_visible : t -> Ident.t -> bool

  val is_module_type_ident_visible : t -> Ident.t -> bool

  val is_module_ident_visible : t -> Ident.t -> bool

end = struct

  type defs =
    | Global of Ident.t
    | Local of Ident.t
    | Unambiguous of Ident.t
    | Ambiguous of Ident.t * Ident.t list

  type t =
    { types : Type.t Ident_map.t;
      class_types : Class_type.t Ident_map.t;
      module_types : Module_type.t Ident_map.t;
      modules : Module.t Ident_map.t;
      type_names : defs String_map.t;
      class_type_names : defs String_map.t;
      module_type_names : defs String_map.t;
      module_names : defs String_map.t; }

  let empty =
    { types = Ident_map.empty;
      class_types = Ident_map.empty;
      module_types = Ident_map.empty;
      modules = Ident_map.empty;
      type_names = String_map.empty;
      class_type_names = String_map.empty;
      module_type_names = String_map.empty;
      module_names = String_map.empty; }

  let previous_type t id =
    match Ident_map.find id t.types with
    | exception Not_found -> None
    | prev ->
      match Type.declaration prev with
      | None -> failwith "Graph.add: type already defined"
      | Some _ as o -> o

  let previous_class_type t id =
    match Ident_map.find id t.class_types with
    | exception Not_found -> None
    | prev ->
      match Class_type.declaration prev with
      | None -> failwith "Graph.add: class type already defined"
      | Some _ as o -> o

  let previous_module_type t id =
    match Ident_map.find id t.module_types with
    | exception Not_found -> None
    | prev ->
      match Module_type.declaration prev with
      | None -> failwith "Graph.add: module type already defined"
      | Some _ as o -> o

  let previous_module t id =
    match Ident_map.find id t.modules with
    | exception Not_found -> None
    | prev ->
      match Module.declaration prev with
      | None -> failwith "Graph.add: module already defined"
      | Some _ as o -> o

  let add_name source id names =
    let name = Ident.name id in
    let defs =
      match source with
      | Component.Global -> Global id
      | Component.Local -> Local id
      | Component.Open -> begin
        match String_map.find name names with
        | exception Not_found -> Unambiguous id
        | Global id' -> Unambiguous id'
        | Local id' -> Ambiguous(id, [id'])
        | Unambiguous id' -> Ambiguous(id, [id'])
        | Ambiguous(id', ids) -> Ambiguous(id, id' :: ids)
      end
    in
    String_map.add name defs names

  let merge_name id names =
    let name = Ident.name id in
    match String_map.find name names with
    | exception Not_found ->
        String_map.add name (Global id) names
    | _ -> names

  let add t descs =
    let rec loop acc diff declarations = function
      | [] -> loop_declarations acc diff declarations
      | Component.Type(origin, id, desc, source, dpr) :: rest ->
          let prev = previous_type acc id in
          let typ = Type.base origin id (Some desc) dpr in
          let types = Ident_map.add id typ acc.types in
          let type_names = add_name source id acc.type_names in
          let item = Diff.Item.Type(id, typ, prev) in
          let diff = item :: diff in
          let acc = { acc with types; type_names } in
          loop acc diff declarations rest
      | Component.Class_type(origin,id, desc, source, dpr) :: rest ->
          let prev = previous_class_type acc id in
          let clty = Class_type.base origin id (Some desc) dpr in
          let class_types = Ident_map.add id clty acc.class_types in
          let class_type_names = add_name source id acc.class_type_names in
          let item = Diff.Item.Class_type(id, clty, prev) in
          let diff = item :: diff in
          let acc = { acc with class_types; class_type_names } in
          loop acc diff declarations rest
      | Component.Module_type(origin,id, desc, source, dpr) :: rest ->
          let prev = previous_module_type acc id in
          let mty = Module_type.base origin id (Some desc) dpr in
          let module_types = Ident_map.add id mty acc.module_types in
          let module_type_names = add_name source id acc.module_type_names in
          let item = Diff.Item.Module_type(id, mty, prev) in
          let diff = item :: diff in
          let acc = { acc with module_types; module_type_names } in
          loop acc diff declarations rest
      | Component.Module(origin,id, desc, source, dpr) :: rest ->
          let prev = previous_module acc id in
          let md = Module.base origin id (Some desc) dpr in
          let modules = Ident_map.add id md acc.modules in
          let module_names = add_name source id acc.module_names in
          let item = Diff.Item.Module(id, md, prev) in
          let diff = item :: diff in
          let acc = { acc with modules; module_names } in
          loop acc diff declarations rest
      | Component.Declare_type(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let type_names =
            (* CR lwhite: This should probably not always be [Global] *)
            add_name Component.Global id acc.type_names
          in
          let acc = { acc with type_names } in
          loop acc diff declarations rest
      | Component.Declare_class_type(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let class_type_names =
            (* CR lwhite: This should probably not always be [Global] *)
            add_name Component.Global id acc.class_type_names
          in
          let acc = { acc with class_type_names } in
          loop acc diff declarations rest
      | Component.Declare_module_type(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let module_type_names =
            (* CR lwhite: This should probably not always be [Global] *)
            add_name Component.Global id acc.module_type_names
          in
          let acc = { acc with module_type_names } in
          loop acc diff declarations rest
      | Component.Declare_module(_, id) as decl :: rest ->
          let declarations = decl :: declarations in
          let module_names =
            (* CR lwhite: This should probably not always be [Global] *)
            add_name Component.Global id acc.module_names
          in
          let acc = { acc with module_names } in
          loop acc diff declarations rest
    and loop_declarations acc diff = function
      | [] -> acc, diff
      | Component.Declare_type(origin, id) :: rest ->
          if Ident_map.mem id acc.types then begin
            loop_declarations acc diff rest
          end else begin
            let typ = Type.declare origin id in
            let types = Ident_map.add id typ acc.types in
            let acc = { acc with types } in
            loop_declarations acc diff rest
          end
      | Component.Declare_class_type(origin, id) :: rest ->
          if Ident_map.mem id acc.class_types then begin
            loop_declarations acc diff rest
          end else begin
            let clty = Class_type.declare origin id in
            let class_types = Ident_map.add id clty acc.class_types in
            let acc = { acc with class_types } in
            loop_declarations acc diff rest
          end
      | Component.Declare_module_type(origin, id) :: rest ->
          if Ident_map.mem id acc.module_types then begin
            loop_declarations acc diff rest
          end else begin
            let mty = Module_type.declare origin id in
            let module_types = Ident_map.add id mty acc.module_types in
            let acc = { acc with module_types } in
            loop_declarations acc diff rest
          end
      | Component.Declare_module(origin, id) :: rest ->
          if Ident_map.mem id acc.modules then begin
            loop_declarations acc diff rest
          end else begin
            let md = Module.declare origin id in
            let modules = Ident_map.add id md acc.modules in
            let acc = { acc with modules } in
            loop_declarations acc diff rest
          end
      | ( Component.Type _
        | Component.Class_type _
        | Component.Module_type _
        | Component.Module _) :: _ -> assert false
    in
    loop t [] [] descs

  let merge t diff =
    let rec loop acc = function
      | [] -> acc
      | Diff.Item.Type(id, typ, _) :: rest ->
          let types = Ident_map.add id typ acc.types in
          let type_names = merge_name id acc.type_names in
          let acc = { acc with types; type_names } in
          loop acc rest
      | Diff.Item.Class_type(id, clty, _) :: rest ->
          let class_types = Ident_map.add id clty acc.class_types in
          let class_type_names = merge_name id acc.class_type_names in
          let acc = { acc with class_types; class_type_names } in
          loop acc rest
      | Diff.Item.Module_type(id, mty, _) :: rest ->
          let module_types = Ident_map.add id mty acc.module_types in
          let module_type_names = merge_name id acc.module_type_names in
          let acc = { acc with module_types; module_type_names } in
          loop acc rest
      | Diff.Item.Module(id, md, _) :: rest ->
          let modules = Ident_map.add id md acc.modules in
          let module_names = merge_name id acc.module_names in
          let acc = { acc with modules; module_names } in
          loop acc rest
    in
    loop t diff

  let rec find_module t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.modules
    | Path.Pdot(p, name) ->
        let md = find_module t p in
        Module.find_module t md name
    | Path.Papply(p, arg) ->
        let md = find_module t p in
        Module.find_application t md arg

  let find_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.types
    | Path.Pdot(p, name) ->
        let md = find_module t p in
        Module.find_type t md name
    | Path.Papply _ ->
        raise Not_found

  let find_class_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.class_types
    | Path.Pdot(p, name) ->
        let md = find_module t p in
        Module.find_class_type t md name
    | Path.Papply _ ->
        raise Not_found

  let find_module_type t path =
    match path with
    | Path.Pident id ->
        Ident_map.find id t.module_types
    | Path.Pdot(p, name) ->
        let md = find_module t p in
        Module.find_module_type t md name
    | Path.Papply _ ->
        raise Not_found

  let canonical_type_path t id =
    match Ident_map.find id t.types with
    | exception Not_found -> Path.Pident id
    | md -> Type.path t md

  let canonical_class_type_path t id =
    match Ident_map.find id t.class_types with
    | exception Not_found -> Path.Pident id
    | md -> Class_type.path t md

  let canonical_module_type_path t id =
    match Ident_map.find id t.module_types with
    | exception Not_found -> Path.Pident id
    | md -> Module_type.path t md

  let canonical_module_path t id =
    match Ident_map.find id t.modules with
    | exception Not_found -> Path.Pident id
    | md -> Module.path t md

  let is_module_ident_visible t id =
    let name = Ident.name id in
    match String_map.find name t.module_names with
    | exception Not_found -> false
    | Local id' -> Ident.equal id id'
    | Global id' -> Ident.equal id id'
    | Unambiguous id' -> Ident.equal id id'
    | Ambiguous(id', ids) ->
      if not (Ident.equal id id') then false
      else begin
        let paths = List.map (canonical_module_path t) ids in
        let path = canonical_module_path t id in
        List.for_all (Path.equal path) paths
      end

  let rec is_module_path_visible t = function
    | Path.Pident id -> is_module_ident_visible t id
    | Path.Pdot(path, _) ->
        is_module_path_visible t path
    | Path.Papply(path1, path2) ->
        is_module_path_visible t path1
        && is_module_path_visible t path2

  let is_type_ident_visible t id =
    let name = Ident.name id in
    match String_map.find name t.type_names with
    | exception Not_found -> false
    | Local id' -> Ident.equal id id'
    | Global id' -> Ident.equal id id'
    | Unambiguous id' -> Ident.equal id id'
    | Ambiguous(id', ids) ->
      if not (Ident.equal id id') then false
      else begin
        let paths = List.map (canonical_type_path t) ids in
        let path = canonical_type_path t id in
        List.for_all (Path.equal path) paths
      end

  let is_type_path_visible t = function
    | Path.Pident id -> is_type_ident_visible t id
    | Path.Pdot(path, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_type_path_visible: \
           invalid type path"

  let is_class_type_ident_visible t id =
    let name = Ident.name id in
    match String_map.find name t.class_type_names with
    | exception Not_found -> false
    | Local id' -> Ident.equal id id'
    | Global id' -> Ident.equal id id'
    | Unambiguous id' -> Ident.equal id id'
    | Ambiguous(id', ids) ->
      if not (Ident.equal id id') then false
      else begin
        let paths = List.map (canonical_class_type_path t) ids in
        let path = canonical_class_type_path t id in
        List.for_all (Path.equal path) paths
      end

  let is_class_type_path_visible t = function
    | Path.Pident id -> is_class_type_ident_visible t id
    | Path.Pdot(path, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_class_type_path_visible: \
           invalid class type path"

  let is_module_type_ident_visible t id =
    let name = Ident.name id in
    match String_map.find name t.module_type_names with
    | exception Not_found -> false
    | Local id' -> Ident.equal id id'
    | Global id' -> Ident.equal id id'
    | Unambiguous id' -> Ident.equal id id'
    | Ambiguous(id', ids) ->
      if not (Ident.equal id id') then false
      else begin
        let paths = List.map (canonical_module_type_path t) ids in
        let path = canonical_module_type_path t id in
        List.for_all (Path.equal path) paths
      end

  let is_module_type_path_visible t = function
    | Path.Pident id -> is_module_type_ident_visible t id
    | Path.Pdot(path, _) -> is_module_path_visible t path
    | Path.Papply _ ->
        failwith
          "Short_paths_graph.Graph.is_module_type_path_visible: \
           invalid module type path"

end

type graph = Graph.t
