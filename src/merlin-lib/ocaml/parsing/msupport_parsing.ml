(* Filled in from Msupport. *)
let msupport_raise_error : (exn -> unit) ref =
  ref raise

let raise_error exn =
  !msupport_raise_error exn
