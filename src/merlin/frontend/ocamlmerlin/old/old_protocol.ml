(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
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

open Std

type protocol_version =
  [ `V2 (* First version to support versioning ! *)
  | `V3 (* Responses are now assoc {class:string, value:..., notifications:string list} *)
  ]

module Context =
struct
  type document = {
    kind: [`ML | `MLI | `Auto ];
    path: string option;
    dot_merlins: string list option;
  }

  type t = {
    document: document option;
    printer_width: int option;
    printer_verbosity: int option;
  }
end

type _ sync_command =
  | Tell
    : Msource.position * Msource.position * string
    -> unit sync_command
  | Refresh
    :  unit sync_command
  | Flags_set
    :  string list
    -> [ `Ok | `Failures of (string * exn) list ] sync_command
  | Findlib_use
    :  string list
    -> [`Ok | `Failures of (string * exn) list] sync_command
  | Extension_set
    :  [`Enabled|`Disabled] * string list
    -> [`Ok | `Failures of (string * exn) list] sync_command
  | Path
    :  [`Build|`Source]
     * [`Add|`Rem]
     * string list
    -> unit sync_command
  | Path_reset
    :  unit sync_command
  | Protocol_version
    : int option
    -> ([`Selected of protocol_version] *
        [`Latest of protocol_version] *
        string) sync_command
  | Checkout
    : Context.document
    -> unit sync_command
  | Idle_job
    : bool sync_command
  | Flags_get
    :  string list sync_command
  | Project_get
    :  (string list * [`Ok | `Failures of string list]) sync_command

type 'a command =
  | Query of 'a Query_protocol.t
  | Sync  of 'a sync_command

type request = Request : Context.t * 'a command -> request

type response =
  | Return    : 'a command * 'a -> response
  | Failure   : string -> response
  | Error     : Json.t -> response
  | Exception : exn -> response
