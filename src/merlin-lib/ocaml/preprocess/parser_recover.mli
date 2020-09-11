open Parser_raw

module Default : sig
  val default_loc : Location.t ref
end

val default_value : 'a MenhirInterpreter.symbol -> 'a

type action =
  | Abort
  | R of int
  | S : 'a MenhirInterpreter.symbol -> action
  | Sub of action list

type decision =
  | Nothing
  | One of action list
  | Select of (int -> action list)

val depth : int array

val can_pop : 'a MenhirInterpreter.terminal -> bool

val recover : int -> decision
