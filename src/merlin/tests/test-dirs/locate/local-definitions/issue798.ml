module List =
  struct
    include List
    let foo l =
      match l with
      | hd :: tl ->
         let _ = hd in
         assert false
      | [] ->
         assert false
  end
