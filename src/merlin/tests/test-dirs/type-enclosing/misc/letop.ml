let (let*) o f = Option.map

let plus_two tbl key =
  let* foo = Hashtbl.find_opt tbl key in
  foo + 2
