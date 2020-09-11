module type S1 = sig
  include module type of Foo

  val foo : int t
end

module type S2 = sig
  include module type of struct include Foo end

  val foo : int t
end
