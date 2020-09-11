module Do_the_thing(Test : Foo.Test.S) = struct
  let stuff () = ()

  include Test
end

include Do_the_thing(Foo.Test)
