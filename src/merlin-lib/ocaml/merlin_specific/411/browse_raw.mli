(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2014  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

(** [Browse_node] offers a uniform interface to traverse constructions from
  * [TypedTree].
  *
  * Mutually recursive types from [TypedTree] are wrapped into different
  * constructors of the type [node].
  * Then the [fold] function traverses one-level of sub-nodes.
  *
  * In the meantime, the most specific environment and location are threaded
  * (FIXME: should these two be managed separately?).
  *
  * Finally [BrowseT] module a node into a tree which structure mimics
  * the recursive structure of the [TypedTree] node.
  *
  *)

(* Compatibility with previous versions of OCaml *)
type constructor_declaration = Typedtree.constructor_declaration

open Typedtree

type node =
  | Dummy
  | Pattern                  : _ general_pattern -> node
  | Expression               of expression
  | Case                     : _ case -> node
  | Class_expr               of class_expr
  | Class_structure          of class_structure
  | Class_field              of class_field
  | Class_field_kind         of class_field_kind
  | Module_expr              of module_expr
  | Module_type_constraint   of module_type_constraint
  | Structure                of structure
  | Signature                of signature
  | (* Items come with their final environment *)
    Structure_item           of structure_item * Env.t
  | Signature_item           of signature_item * Env.t
  | Module_binding           of module_binding
  | Value_binding            of value_binding
  | Module_type              of module_type
  | Module_declaration       of module_declaration
  | Module_type_declaration  of module_type_declaration
  | With_constraint          of with_constraint
  | Core_type                of core_type
  | Package_type             of package_type
  | Row_field                of row_field
  | Value_description        of value_description
  | Type_declaration         of type_declaration
  | Type_kind                of type_kind
  | Type_extension           of type_extension
  | Extension_constructor    of extension_constructor
  | Label_declaration        of label_declaration
  | Constructor_declaration  of constructor_declaration
  | Class_type               of class_type
  | Class_signature          of class_signature
  | Class_type_field         of class_type_field
  | Class_declaration        of class_declaration
  | Class_description        of class_description
  | Class_type_declaration   of class_type_declaration

  | Include_description      of include_description
  | Include_declaration      of include_declaration
  | Open_description         of open_description
  | Open_declaration         of open_declaration

  | Method_call              of expression * meth * Location.t
  | Record_field             of [ `Expression of expression | `Pattern of pattern ] *
                                Types.label_description * Location.t
  | Module_binding_name      of module_binding
  | Module_declaration_name  of module_declaration
  | Module_type_declaration_name of module_type_declaration

val fold_node : (Env.t -> node -> 'a -> 'a) -> Env.t -> node -> 'a -> 'a

(** Accessors for information specific to a node *)

val node_update_env : Env.t -> node -> Env.t
val node_real_loc : Location.t -> node -> Location.t
val node_merlin_loc : Location.t -> node -> Location.t
val node_attributes : node -> attribute list

val string_of_node : node -> string

val node_paths : node -> Path.t Location.loc list

val node_is_constructor : node ->
  [ `Description of Types.constructor_description
  | `Declaration of Typedtree.constructor_declaration ] Location.loc option

val node_of_binary_part : Env.t -> Cmt_format.binary_part -> node
