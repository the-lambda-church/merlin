type signature_elt =
| Item of Types.signature_item
| Type of Asttypes.rec_flag * Parsetree.type_declaration list

val module_type : Types.module_type -> Parsetree.module_type

val core_type : Types.type_expr -> Parsetree.core_type

val modtype_declaration :
  Ident.t ->
  Types.modtype_declaration ->
  Parsetree.module_type_declaration

val module_declaration :
  Ident.t -> Types.module_declaration -> Parsetree.module_declaration

val signature_item : Types.signature_item -> Parsetree.signature_item

val extension_constructor :
  Ident.t -> Types.extension_constructor -> Parsetree.extension_constructor

val value_description :
  Ident.t -> Types.value_description -> Parsetree.value_description

val label_declaration : Types.label_declaration -> Parsetree.label_declaration

val constructor_arguments :
  Types.constructor_arguments -> Parsetree.constructor_arguments

val constructor_declaration :
  Types.constructor_declaration -> Parsetree.constructor_declaration

val type_declaration :
  Ident.t -> Types.type_declaration -> Parsetree.type_declaration

val signature : Types.signature ->  Parsetree.signature

(** [group_items sig_items] groups items from a signature in a more meaningful
  way: type declaration of the same recursive type are group together and items
  following a class or class_type items are discarded *)
val group_items : Types.signature_item list -> signature_elt list
