open Extend_protocol

module Description : sig
  type t
  val make_v0 : name:string -> version:string -> t
end

module Utils : sig
  val notify : string -> unit
  val debug : string -> unit
end

module Reader : sig
  type t
  val make_v0 : (module Reader.V0) -> t
end

module Handshake : sig
  val magic_number : string

  type versions = {
    ast_impl_magic_number : string;
    ast_intf_magic_number : string;
    cmi_magic_number : string;
    cmt_magic_number : string;
  }

  val versions : versions

  val negotiate_driver : string -> in_channel -> out_channel -> capabilities
end

(** The main entry point of an extension. *)
val extension_main : ?reader:Reader.t -> Description.t -> 'a
