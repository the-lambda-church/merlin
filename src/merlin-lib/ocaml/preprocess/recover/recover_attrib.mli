open MenhirSdk

module type S = sig
  module G : Cmly_api.GRAMMAR

  val cost_of_prod    : G.production -> float
  val penalty_of_item : G.production * int -> float
  val cost_of_symbol  : G.symbol -> float

  val default_prelude     : Format.formatter -> unit
  val default_terminal    : G.terminal -> string option
  val default_nonterminal : G.nonterminal -> string option
end

module Make (G : Cmly_api.GRAMMAR) : S with module G = G
