module Make(X : Map.OrderedType) : sig
  include Map.OrderedType with type t = X.t
end
