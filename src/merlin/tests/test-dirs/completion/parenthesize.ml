module MyList = struct
  [@@@ocaml.warning "-65"]
  type 'a t =
    | (::) of 'a * 'a t
    | []
  type u = ()
  let (mod) = ()
  let random = 1
end

let _ = MyList.
