module P = Extend_protocol
module R = P.Reader

module Description = struct
  type t = P.description

  let make_v0 ~name ~version = { P. name; version }
end

module Reader = struct
  type t = (module R.V0)
  let make_v0 (x : (module R.V0)) : t = x

  module Make (V : R.V0) = struct

    open P.Reader

    let buffer = ref None

    let get_buffer () =
      match !buffer with
      | None -> invalid_arg "No buffer loaded"
      | Some buffer -> buffer

    let exec = function
      | Req_load buf ->
        buffer := Some (V.load buf);
        Res_loaded
      | Req_parse ->
        Res_parse (V.parse (get_buffer ()))
      | Req_parse_line (pos, str) ->
        Res_parse (V.parse_line (get_buffer ()) pos str)
      | Req_parse_for_completion pos ->
        let info, tree = V.for_completion (get_buffer ()) pos in
        Res_parse_for_completion (info, tree)
      | Req_get_ident_at pos ->
        Res_get_ident_at (V.ident_at (get_buffer ()) pos)
      | Req_print_outcome trees ->
        let print t =
          V.print_outcome Format.str_formatter t;
          Format.flush_str_formatter ()
        in
        let trees = List.rev_map print trees in
        Res_print_outcome (List.rev trees)
      | Req_pretty_print p ->
        V.pretty_print Format.str_formatter p;
        Res_pretty_print (Format.flush_str_formatter ())

  end
end

module Utils = struct

  (* Postpone messages until ready *)
  let send, set_ready =
    let is_ready = ref false in
    let postponed = ref [] in
    let really_send msg = output_value stdout msg in
    let set_ready () =
      is_ready := true;
      let postponed' = List.rev !postponed in
      postponed := [];
      List.iter really_send postponed'
    in
    let send msg =
      if !is_ready then
        really_send msg
      else
        postponed := msg :: !postponed
    in
    send, set_ready

  let notify msg = send (P.Notify msg)
  let debug msg = send (P.Debug msg)
end

module Handshake = struct
  let magic_number : string = "MERLINEXTEND002"

  type versions = {
    ast_impl_magic_number : string;
    ast_intf_magic_number : string;
    cmi_magic_number : string;
    cmt_magic_number : string;
  }

  let versions = Config.({
      ast_impl_magic_number;
      ast_intf_magic_number;
      cmi_magic_number;
      cmt_magic_number;
    })

 let negotiate (capabilities : P.capabilities) =
    output_string stdout magic_number;
    output_value stdout versions;
    output_value stdout capabilities;
    flush stdout;
    Utils.set_ready ();
    match input_value stdin with
    | exception End_of_file -> exit 0
    | P.Start_communication -> ()
    | _ ->
      prerr_endline "Unexpected value after handshake.";
      exit 1

  let negotiate_driver ext_name i o =
    let magic' = really_input_string i (String.length magic_number) in
    if magic' <> magic_number then (
      let msg = Printf.sprintf
          "Extension %s has incompatible protocol version %S (expected %S)"
          ext_name magic' magic_number
      in
      failwith msg
    );
    let versions' : versions = input_value i in
    let check_v prj name =
      if prj versions <> prj versions' then
        let msg = Printf.sprintf
            "Extension %s %s has incompatible version %S (expected %S)"
            ext_name name (prj versions') (prj versions)
        in
        failwith msg
    in
    check_v (fun x -> x.ast_impl_magic_number) "implementation AST";
    check_v (fun x -> x.ast_intf_magic_number) "interface AST";
    check_v (fun x -> x.cmi_magic_number) "compiled interface (CMI)";
    check_v (fun x -> x.cmt_magic_number) "typedtree (CMT)";
    output_value o P.Start_communication;
    flush o;
    let capabilities : P.capabilities =
      input_value i
    in
    capabilities
end

(** The main entry point of an extension. *)
let extension_main ?reader desc =
  (* Check if invoked from Merlin *)
  begin match Sys.getenv "__MERLIN_MASTER_PID" with
  | exception Not_found ->
    Printf.eprintf "This is %s merlin extension, version %s.\n\
                    This binary should be invoked from merlin and \
                    cannot be used directly.\n%!"
      desc.P.name
      desc.P.version;
    exit 1;
  | _ -> ()
  end;
  (* Communication happens on stdin/stdout. *)
  Handshake.negotiate {P. reader = reader <> None};
  let reader = match reader with
    | None -> (fun _ -> failwith "No reader")
    | Some (module R : R.V0) ->
      let module M = Reader.Make(R) in
      M.exec
  in
  let respond f =
    match f () with
    | (r : P.response) -> Utils.send r
    | exception exn ->
      let name = Printexc.exn_slot_name exn in
      let desc = Printexc.to_string exn in
      Utils.send (P.Exception (name, desc))
  in
  let rec loop () =
    flush stdout;
    match input_value stdin with
    | exception End_of_file -> exit 0
    | P.Start_communication ->
      prerr_endline "Unexpected message.";
      exit 2
    | P.Reader_request request ->
      respond (fun () -> P.Reader_response (reader request));
      loop ()
  in
  loop ()
