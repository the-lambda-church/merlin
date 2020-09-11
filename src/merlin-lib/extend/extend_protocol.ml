module Reader = struct

  (** Description of a buffer managed by Merlin *)
  type buffer = {

    path : string;
    (** Path of the buffer in the editor.
        The path is absolute if it is backed by a file, although it might not yet
        have been saved in the editor.
        The path is relative if it is a temporary buffer. *)

    flags : string list;
    (** Any flag that has been passed to the reader in .merlin file *)

    text : string;
    (** Content of the buffer *)
  }

  (** ASTs exchanged with Merlin *)
  type parsetree =

    | Structure of Parsetree.structure
      (** An implementation, usually coming from a .ml file *)

    | Signature of Parsetree.signature
      (** An interface, usually coming from a .mli file *)

  (** Printing in error messages or completion items *)
  type outcometree =
    | Out_value          of Outcometree.out_value
    | Out_type           of Outcometree.out_type
    | Out_class_type     of Outcometree.out_class_type
    | Out_module_type    of Outcometree.out_module_type
    | Out_sig_item       of Outcometree.out_sig_item
    | Out_signature      of Outcometree.out_sig_item list
    | Out_type_extension of Outcometree.out_type_extension
    | Out_phrase         of Outcometree.out_phrase

  (** Printing in case destruction *)
  type pretty_parsetree =
    | Pretty_toplevel_phrase of Parsetree.toplevel_phrase
    | Pretty_expression      of Parsetree.expression
    | Pretty_core_type       of Parsetree.core_type
    | Pretty_pattern         of Parsetree.pattern
    | Pretty_signature       of Parsetree.signature
    | Pretty_structure       of Parsetree.structure
    | Pretty_case_list       of Parsetree.case list

  (** Additional information useful for guiding completion *)
  type complete_info = {
    complete_labels : bool;
    (** True if it is appropriate to suggest labels for this completion. *)
  }

  module type V0 = sig
    (** Internal representation of a buffer for the extension.
        Extension should avoid global state, cached information should be stored
        in values of this type. *)
    type t

    (** Turns a merlin-buffer into an internal buffer.

        This function should be total, an exception at this point is a
        fatal-error.

        Simplest implementation is identity, with type t = buffer.
    *)
    val load : buffer -> t

    (** Get the main parsetree from the buffer.
        This should return the AST corresponding to [buffer.source].
    *)
    val parse : t -> parsetree

    (** Give the opportunity to optimize the parsetree when completing from a
        specific position.

        The simplest implementation is:

            let for_completion t _ = ({complete_labels = true}, (tree t))

        But it might be worthwhile to specialize the parsetree for a better
        completion.
    *)
    val for_completion : t -> Lexing.position -> complete_info * parsetree

    (** Parse a separate user-input in the context of this buffer.
        Used when the user manually enters an expression and ask for its type or location.
    *)
    val parse_line : t -> Lexing.position -> string -> parsetree

    (** Given a buffer and a position, return the components of the identifier
        (actually the qualified path) under the cursor.

        This should return the raw identifier names -- operators should not be
        surrounded by parentheses.

        An empty list is a valid result if no identifiers are under the cursor.
    *)
    val ident_at : t -> Lexing.position -> string Location.loc list

    (** Opposite direction: pretty-print a tree.
        This works on outcometree and is used for displaying answers to queries.
        (type errors, signatures of modules in environment, completion candidates, etc).
    *)
    val print_outcome : Format.formatter -> outcometree -> unit

    (* This one works on parsetree and is used for case destruction
       (merlin-destruct) *)
    val pretty_print : Format.formatter -> pretty_parsetree -> unit
  end

  type request =
    | Req_load of buffer
    | Req_parse
    | Req_parse_line of Lexing.position * string
    | Req_parse_for_completion of Lexing.position
    | Req_get_ident_at of Lexing.position
    | Req_print_outcome of outcometree list
    | Req_pretty_print of pretty_parsetree

  type response =
    | Res_loaded
    | Res_parse of parsetree
    | Res_parse_for_completion of complete_info * parsetree
    | Res_get_ident_at of string Location.loc list
    | Res_print_outcome of string list
    | Res_pretty_print of string

end

(* Name of the extension *)
type description = {
  name : string;
  version : string;
}

(* Services an extension can provide *)
type capabilities = {
  reader: bool;
}

(* Main protocol *)
type request =
  | Start_communication
  | Reader_request of Reader.request

type response =
  | Notify of string
  | Debug of string
  | Exception of string * string
  | Reader_response of Reader.response
