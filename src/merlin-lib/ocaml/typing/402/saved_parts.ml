let attribute = Location.mknoloc "merlin.saved-parts"

let table = Hashtbl.create 7

let gensym =
  let counter = ref 0 in
  fun () -> incr counter; !counter

let finalize = function
  | Asttypes.Const_int id ->
    Hashtbl.remove table id;
  | _ -> assert false

let store parts =
  let id = gensym () in
  let key = Asttypes.Const_int id in
  Gc.finalise finalize key;
  Hashtbl.add table id parts;
  key

let find = function
  | Asttypes.Const_int id ->
    begin
      try Hashtbl.find table id
      with Not_found -> []
    end
  | _ -> assert false
