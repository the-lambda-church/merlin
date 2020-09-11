(** Support for creating fresh types isomorphic to the natural numbers *)

(** Module type for arrays indexed by a type [index] *)
module type Array = sig

  type index

  type 'a t

  val empty : 'a t

  val singleton : 'a -> 'a t

  val extend : 'a t -> index -> (index -> 'a) -> 'a t

  val retract : 'a t -> index -> 'a t

  val contains : 'a t -> index -> bool

  val last : 'a t -> index option

  val set : 'a t -> index -> 'a -> unit

  val get : 'a t -> index -> 'a

end

(** Module type for types isomorphic to the natural numbers
    without zero (up to [maximum]) *)
module type S_no_zero = sig

  type t

  val one : t

  val maximum : t

  val succ : t -> t

  val pred : t -> t option

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val less_than : t -> t -> bool

  val less_than_or_equal : t -> t -> bool

  val max : t -> t -> t

  val plus : t -> t -> t

  val pp : Format.formatter -> t -> unit

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t

  module Tbl : Hashtbl.S with type key = t

  module Array : Array with type index = t

end


(** Module type for types isomorphic to the natural numbers
    (up to [maximum]) *)
module type S = sig

  include S_no_zero

  val zero : t

end

(** Functor to create fresh types isomorphic to the natural numbers *)
module Make () : S

(** Functor to create fresh types isomorphic to the natural numbers
    without zero *)
module Make_no_zero () : S_no_zero

