let attribute = Location.mknoloc "merlin.saved-parts"

module H = Ephemeron.K1.Make(struct
    type t = string
    let hash = Hashtbl.hash
    let equal (a : t) (b : t) =  a = b
  end)

let table = H.create 7

let gensym =
  let counter = ref 0 in
  fun () -> incr counter; !counter

let store parts =
  let id = string_of_int (gensym ()) in
  let key = Parsetree.Pconst_integer (id, None) in
  H.add table id parts;
  key

let find = function
  | Parsetree.Pconst_integer (id, None) ->
    begin
      try H.find table id
      with Not_found -> []
    end
  | _ -> assert false
