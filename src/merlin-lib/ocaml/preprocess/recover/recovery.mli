open MenhirSdk.Cmly_api
module type S = sig
  module G : GRAMMAR

  type item = G.lr1 * G.production * int
  type recovery = G.lr1 -> int * (G.lr1 option * item list) list

  val recover : recovery
  val report : Format.formatter -> unit
end

module Make (G : GRAMMAR) (S : Synthesis.S with module G = G) : S with module G = G
