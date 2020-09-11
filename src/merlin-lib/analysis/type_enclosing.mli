(** Provides type information around the cursor.

    The information comes from two sources:
    1. enclosing AST nodes: we just retrieve the types in the typedtree
    2. if the cursor is on an identifier, by typing it in the current
    environment

    (2) is primarily useful in the following situations:
    - when the identifier is polymorphic in the environment, but monomorphic in
    the AST because it's been instantiated.
    - when there is a syntax or type error in that area, and we don't have a
    precise enough AST node for the position (i.e. we got a "recovered" node, of
    type ['a]).

    Furthermore, (2) has a finer granularity than (1): when the cursor is in the
    middle of a longident, e.g. [Foo.B|ar.Baz.lol] (with | being the cursor),
    then we'll have one AST node covering the whole ident.
    But what we reconstruct gives us: [Foo.Bar], [Foo.Bar.Baz],
    [Foo.Bar.Baz.lol]; and we return the type for each of them.
    These are what we call "small enclosings".

    There are however some issues with the small enclosings:
    - one has to be careful of the context (obviously that information won't be
    available in case of parse errors); because a given identifier could exist
    in different namespaces, for instance:
    {[
      type t
      module type t = sig val t : t end
      let t (t : t) : (module t) = (module struct let t = t end)
    ]}

    - the information might be redundant with the one we get from the AST.
*)

val log_section : string

type type_info =
  | Modtype of Env.t * Types.module_type
  | Type of Env.t * Types.type_expr
  | Type_decl of Env.t * Ident.t * Types.type_declaration
  | String of string

type typed_enclosings =
  (Location.t * type_info * Query_protocol.is_tail_position) list

val from_nodes :
  path:(Env.t * Browse_raw.node * Query_protocol.is_tail_position) list ->
  typed_enclosings

val from_reconstructed :
  nodes:(Env.t * Browse_raw.node) list ->
  cursor:Lexing.position ->
  verbosity:int ->
  string Location.loc list ->
  typed_enclosings
