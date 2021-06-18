###############
## PREFIXING ##
###############

Test 1.1 :

  $ cat >c1.ml <<EOF
  > module Prefix = struct
  >   type t = A of int | B
  > end
  > let x : Prefix.t = _
  > EOF

  $ $MERLIN single construct -position 4:20 -filename c1.ml <c1.ml |
  >  jq ".value"
  [
    {
      "start": {
        "line": 4,
        "col": 19
      },
      "end": {
        "line": 4,
        "col": 20
      }
    },
    [
      "Prefix.B",
      "(Prefix.A _)"
    ]
  ]

Test 1.2 :

  $ cat >c12.ml <<EOF
  > module Prefix = struct
  >   type t = A of int | B
  > end
  > open Prefix
  > let x : t = _
  > EOF

  $ $MERLIN single construct -position 5:13 -filename c12.ml <c12.ml |
  >  jq ".value"
  [
    {
      "start": {
        "line": 5,
        "col": 12
      },
      "end": {
        "line": 5,
        "col": 13
      }
    },
    [
      "B",
      "(A _)"
    ]
  ]

Test 1.3 :

  $ cat >c13.ml <<EOF
  > module Prefix = struct
  >   type t = A of int | B
  >   type r = { a : t }
  > end
  > let x : Prefix.t = _
  > let x : Prefix.r = _
  > open Prefix
  > let x : t = _
  > let x : r = _
  > EOF

  $ $MERLIN single construct -position 5:20 -filename c13.ml <c13.ml |
  >  jq ".value[1]"
  [
    "Prefix.B",
    "(Prefix.A _)"
  ]

  $ $MERLIN single construct -position 6:20 -filename c13.ml <c13.ml |
  >  jq ".value[1]"
  [
    "{ Prefix.a = _ }"
  ]

  $ $MERLIN single construct -position 8:13 -filename c13.ml <c13.ml |
  >  jq ".value[1]"
  [
    "B",
    "(A _)"
  ]

  $ $MERLIN single construct -position 9:13 -filename c13.ml <c13.ml |
  >  jq ".value[1]"
  [
    "{ a = _ }"
  ]

