Test 1
  $ cat >fun1.ml <<EOF
  > module Mymod = struct type the_type = int end
  > type the_type = float
  > let x : Mymod.the_type -> the_type -> unit =
  >   _
  > EOF

  $ $MERLIN single construct -position 4:2 \
  > -filename fun1.ml <fun1.ml | jq ".value"
  [
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 4,
        "col": 3
      }
    },
    [
      "(fun the_type the_type_1 -> _)"
    ]
  ]

Test 2
  $ cat >fun2.ml <<EOF
  > module Mymod = struct type int = string end
  > type int = float
  > let x : Mymod.int -> int -> unit =
  >   _
  > EOF

  $ $MERLIN single construct -position 4:2 \
  > -filename fun2.ml <fun2.ml | jq ".value"
  [
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 4,
        "col": 3
      }
    },
    [
      "(fun int int_1 -> _)"
    ]
  ]

Test 3
  $ cat >fun3.ml <<EOF
  > module Mymod : 
  >  sig type t val x : t val f : int -> t end =
  >  struct type t = int let x = 3 let f x = 2 * x end
  > type t = float
  > let g x = 2 *. x 
  > let x : Mymod.t = 3
  > let z : Mymod.t =
  >   _
  > let t : t =
  >   _
  > EOF

Here nothing is expected as t is abstract
  $ $MERLIN single construct -position 8:2 \
  > -filename fun3.ml <fun3.ml | jq ".value[1]"
  []

  $ $MERLIN single construct -position 8:2 -with-values local \
  > -filename fun3.ml <fun3.ml | jq ".value[1]"
  [
    "x",
    "(Mymod.f _)",
    "Mymod.x"
  ]

  $ $MERLIN single construct -position 10:2 \
  > -filename fun3.ml <fun3.ml | jq ".value[1]"
  [
    "0.0"
  ]

  $ $MERLIN single construct -position 10:2 -with-values local \
  > -filename fun3.ml <fun3.ml | jq ".value[1]"
  [
    "0.0",
    "(g _)"
  ]
