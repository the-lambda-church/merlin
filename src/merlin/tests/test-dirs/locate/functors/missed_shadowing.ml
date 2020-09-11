module W = struct end

module M (W : sig end) : sig end = struct
  include W
end

module type X = sig end

module N (X : X) : sig end = struct
  include X
end
