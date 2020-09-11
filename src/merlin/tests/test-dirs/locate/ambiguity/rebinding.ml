module X = struct
  module Y = struct
    module Z = struct
      let foo () = ()
    end
  end
end

open X.Y

let () = Z.foo ()

module X = X.Y

let () = Z.foo ()
