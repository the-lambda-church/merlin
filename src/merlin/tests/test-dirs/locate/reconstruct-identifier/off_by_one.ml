module Wx = struct type t end

module type Q = sig
  val f : x:Wx.t -> unit
end
