module P = Extend_protocol

(** Helper for the driver (Merlin) *)

type t = {
  name: string;
  capabilities: P.capabilities;
  stdin: out_channel;
  stdout: in_channel;
  mutable pid: int;

  notify: string -> unit;
  debug: string -> unit;
}

exception Extension of string * string * string

let run ?(notify=ignore) ?(debug=ignore) name =
  let pstdin, stdin = Unix.pipe () in
  let stdout, pstdout = Unix.pipe () in
  Unix.set_close_on_exec pstdin;
  Unix.set_close_on_exec stdin;
  Unix.set_close_on_exec pstdout;
  Unix.set_close_on_exec stdout;
  let pid =
    Unix.create_process
      ("ocamlmerlin-" ^ name) [||]
      pstdin pstdout Unix.stderr
  in
  Unix.close pstdout;
  Unix.close pstdin;
  let stdin  = Unix.out_channel_of_descr stdin in
  let stdout = Unix.in_channel_of_descr stdout in
  match Extend_main.Handshake.negotiate_driver name stdout stdin with
  | capabilities -> {name; capabilities; stdin; stdout; pid; notify; debug}
  | exception exn ->
    close_out_noerr stdin;
    close_in_noerr stdout;
    raise exn

let stop t =
  close_out_noerr t.stdin;
  close_in_noerr t.stdout;
  if t.pid <> -1 then (
    let _, _ = Unix.waitpid [] t.pid in
    t.pid <- -1;
  )

let capabilities t = t.capabilities

let reader t request =
  if t.pid = -1 then
    invalid_arg "Extend_main.Driver.reader: extension is closed";
  output_value t.stdin (P.Reader_request request);
  flush t.stdin;
  let rec aux () =
    match input_value t.stdout with
    | P.Notify str -> t.notify str; aux ()
    | P.Debug str -> t.debug str; aux ()
    | P.Exception (kind, msg) ->
      stop t;
      raise (Extension (t.name, kind, msg))
    | P.Reader_response response ->
      response
  in
  aux ()
