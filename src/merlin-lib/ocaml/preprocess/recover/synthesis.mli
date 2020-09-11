open MenhirSdk.Cmly_api

module type S = sig
  module G : GRAMMAR

  type variable =
    | Head of G.lr1 * G.nonterminal
    | Tail of G.lr1 * G.production * int

  val variable_to_string : variable -> string

  type 'a paction =
    | Abort
    | Reduce of G.production
    | Shift  of G.symbol
    | Var    of 'a

  val paction_to_string : ('a -> string) -> 'a paction -> string

  type action = variable paction

  val action_to_string : action -> string

  val pred : G.lr1 -> G.lr1 list

  val cost_of  : variable -> float
  val cost_of_action  : action -> float
  val cost_of_actions : action list -> float
  val solution : variable -> action list
  val report   : Format.formatter -> unit
end

module Make (G : GRAMMAR) (A : Recover_attrib.S with module G = G) : S with module G = G
