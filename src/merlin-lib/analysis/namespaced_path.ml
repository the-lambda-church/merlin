open Std

module Namespace = struct
  type t = [
    | `Vals
    | `Type
    | `Constr
    | `Mod
    | `Modtype
    | `Functor
    | `Labels
    | `Unknown
    | `Apply
  ]

  let to_tag_string = function
    | `Mod -> ""
    | `Functor -> "[functor]"
    | `Labels -> "[label]"
    | `Constr -> "[cstr]"
    | `Type -> "[type]"
    | `Vals -> "[val]"
    | `Modtype -> "[Mty]"
    | `Unknown -> "[?]"
    | `Apply -> "[functor application]"

  let to_string = function
    | `Mod -> "(module) "
    | `Functor -> "(functor)"
    | `Labels -> "(label) "
    | `Constr -> "(constructor) "
    | `Type -> "(type) "
    | `Vals -> "(value) "
    | `Modtype -> "(module type) "
    | `Unknown -> "(unknown)"
    | `Apply -> "(functor application)"
end

module Id = struct
  type t =
    | Id of Ident.t
    | String of string

  let name = function
    | Id id -> Ident.name id
    | String s -> s

  let unique_name = function
    | Id id -> Ident.unique_toplevel_name id
    | String s -> s

  let equal mi1 mi2 =
    match mi1, mi2 with
    | Id i1, Id i2 -> Ident.equal i1 i2
    | Id i, String s
    | String s, Id i -> (Ident.name i) = s
    | String s1, String s2 -> s1 = s2
end

type t = elt list
and elt =
  | Ident of Id.t * Namespace.t
  | Applied_to of t

let rec to_string ~name = function
  | []
  | Applied_to _ :: _ -> invalid_arg "Namespaced_path.to_string"
  | Ident (id, ns) :: rest ->
    List.fold_left rest ~init:(name id ^ Namespace.to_tag_string ns) ~f:(
      fun acc elt ->
        match elt with
        | Ident (id, ns) ->
          Printf.sprintf "%s.%s%s" acc (name id) (Namespace.to_tag_string ns)
        | Applied_to arg ->
          Printf.sprintf "%s(%s)" acc (to_string ~name arg)
    )

let to_unique_string l = to_string ~name:Id.unique_name l
let to_string l = to_string ~name:Id.name l

let of_path ~namespace p =
  let rec aux namespace acc p =
    let open Path in
    match Nopos.view p with
    | Pident id -> Ident (Id.Id id, namespace) :: acc
    | Pdot (p, s) -> aux `Mod (Ident (Id.String s, namespace) :: acc) p
    | Papply (p1, p2) ->
      let acc =
        Applied_to (aux `Mod [] p2) :: acc
      in
      aux `Mod acc p1
  in
  aux namespace [] p

let head_exn = function
  | [] -> invalid_arg "head"
  | x :: _ -> x

let head x =
  try Some (head_exn x)
  with Invalid_argument _ -> None

let peal_head_exn = function
  | [] -> invalid_arg "peal_head_exn"
  | _head :: rest -> rest

let peal_head p =
  try Some (peal_head_exn p)
  with Invalid_argument _ -> None

let rec equal p1 p2 = List.for_all2 ~f:equal_elt p1 p2
and equal_elt elt1 elt2 =
  match elt1, elt2 with
  | Ident (i1, ns1), Ident (i2, ns2) -> Id.equal i1 i2 && ns1 = ns2
  | Applied_to p1, Applied_to p2 -> equal p1 p2
  | _, _ -> false

let rewrite_head ~new_prefix p = new_prefix @ p

let strip_stamps =
  List.map ~f:(function
    | Ident (Id i, ns) -> Ident (String (Ident.name i), ns)
    | elt -> elt
  )

let empty = []

let rec subst_prefix ~old_prefix ~new_prefix p =
  match old_prefix, p with
  | [], _ -> Some (new_prefix @ p)
  | op1 :: ops, elt1 :: p when equal_elt op1 elt1 ->
    subst_prefix ~old_prefix:ops ~new_prefix p
  | _ -> None
