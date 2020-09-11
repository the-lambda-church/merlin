let string = "s"

let foo =
  let _ = {|  Look at this: '\
Foo|} in
  let () = ignore string in
  "Onoes,  Foo_bar_lol baz"
;;
