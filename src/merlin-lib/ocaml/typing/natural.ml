
module type Array = sig

  type index

  type 'a t

  val empty : 'a t

  val singleton : 'a -> 'a t

  val extend : 'a t -> index -> (index -> 'a) -> 'a t

  val retract : 'a t -> index -> 'a t

  val contains : 'a t -> index -> bool

  val last : 'a t -> index option

  val set : 'a t -> index -> 'a -> unit

  val get : 'a t -> index -> 'a

end

module type S_no_zero = sig

  type t

  val one : t

  val maximum : t

  val succ : t -> t

  val pred : t -> t option

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val less_than : t -> t -> bool

  val less_than_or_equal : t -> t -> bool

  val max : t -> t -> t

  val plus : t -> t -> t

  val pp : Format.formatter -> t -> unit

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t

  module Tbl : Hashtbl.S with type key = t

  module Array : Array with type index = t

end

module type S = sig

  include S_no_zero

  val zero : t

end

module IntOps = struct

  type t = int

  let compare (x : t) (y : t) =
    compare x y

  let equal (x : t) (y : t) =
    x = y

  let less_than (x : t) (y : t) =
    x < y

  let less_than_or_equal (x : t) (y : t) =
    x <= y

  let max (x : t) (y : t) =
    if x >= y then x
    else y

  let hash = Hashtbl.hash

  let pp ppf x = Format.pp_print_int ppf x

end

module IntMap = Map.Make(IntOps)

module IntSet = Set.Make(IntOps)

module IntTbl = Hashtbl.Make(IntOps)

module Array_zero_indexed = struct

  type index = int

  type 'a t = 'a array

  let empty = [| |]

  let singleton x = [| x |]

  let extend t idx init =
    let len = idx + 1 in
    let old_len = Array.length t in
    if old_len > idx then
      failwith "Natural.Array.extend: array already contains index";
    if old_len = 0 then begin
      Array.init len init
    end else begin
      let extended = Array.make len (t.(0)) in
      Array.blit t 0 extended 0 old_len;
      for i = old_len to idx do
        Array.unsafe_set extended i (init i)
      done;
      extended
    end

  let retract t idx =
    let old_len = Array.length t in
    if old_len <= idx then
      failwith "Natural.Array.retract: array already doesn't contain index";
    Array.sub t 0 idx

  let contains t idx =
    let len = Array.length t in
    idx < len

  let last t =
    let len = Array.length t in
    if len = 0 then None
    else Some (len - 1)

  let set t idx data =
    t.(idx) <- data

  let get t idx =
    t.(idx)

end

module Array_one_indexed = struct

  type index = int

  type 'a t = 'a array

  let empty = [| |]

  let singleton x = [| x |]

  let extend t idx init =
    let old_len = Array.length t in
    if old_len >= idx then
      failwith "Natural.Array.extend: array already contains index";
    if old_len = 0 then begin
      let initial = init 1 in
      let res = Array.make idx initial in
      for i = 1 to (idx - 1) do
        Array.unsafe_set res i (init (i + 1))
      done;
      res
    end else begin
      let extended = Array.make idx (t.(0)) in
      Array.blit t 0 extended 0 old_len;
      for i = old_len to (idx - 1) do
        Array.unsafe_set extended i (init (i + 1))
      done;
      extended
    end

  let retract t idx =
    let old_len = Array.length t in
    if old_len < idx then
      failwith "Natural.Array.retract: array already doesn't contain index";
    Array.sub t 0 (idx - 1)

  let contains t idx =
    let len = Array.length t in
    idx <= len

  let last t =
    let len = Array.length t in
    if len = 0 then None
    else Some len

  let set t idx data =
    t.(idx - 1) <- data

  let get t idx =
    t.(idx - 1)

end

module Nat = struct

  include IntOps

  let zero = 0

  let one = 1

  let maximum = max_int

  let succ t =
    if t = maximum then t
    else t + 1

  let pred t =
    if t = 0 then None
    else Some (t - 1)

  let plus t1 t2 =
    let res = t1 + t2 in
    if res < 0 then maximum
    else res

  module Map = IntMap

  module Set = IntSet

  module Tbl = IntTbl

  module Array = Array_zero_indexed

end

module Nat_no_zero = struct

  include IntOps

  let one = 1

  let maximum = max_int

  let succ t =
    if t = maximum then t
    else t + 1

  let pred t =
    if t = 1 then None
    else Some (t - 1)

  let plus t1 t2 =
    let res = t1 + t2 in
    if res < 0 then maximum
    else res

  module Map = IntMap

  module Set = IntSet

  module Tbl = IntTbl

  module Array = Array_one_indexed

end

module Make () = Nat

module Make_no_zero () = Nat_no_zero
