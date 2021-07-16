val x : int

module M : sig
  val y : float
end

module F (X : module type of M) (Y : sig val z : string end) : sig
  include module type of Y
  val y : float
end