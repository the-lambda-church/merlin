type core = [ `A | `B ]
type more = [ core | `C ]

let x : more = `A
let y : core = `B

let () = match x with
| `C -> ()
| #core -> ()
