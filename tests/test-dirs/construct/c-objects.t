  $ cat >obj1.ml <<EOF
  > let o : < a : string; get : int -> int option >
  >   = _
  > EOF

  $ $MERLIN single construct -position 2:4 \
  > -filename obj1.ml <obj1.ml | jq ".value"
  [
    {
      "start": {
        "line": 2,
        "col": 4
      },
      "end": {
        "line": 2,
        "col": 5
      }
    },
    [
      "object method get = _ method a = _ end"
    ]
  ]

  $ $MERLIN single construct -depth 2 -position 2:4 \
  > -filename obj1.ml <obj1.ml | jq ".value[1]"
  [
    "object method get int = _ method a = \"\" end"
  ]

  $ $MERLIN single construct -depth 3 -position 2:4 \
  > -filename obj1.ml <obj1.ml | jq ".value[1]"
  [
    "object method get int = Some _ method a = \"\" end",
    "object method get int = None method a = \"\" end"
  ]

More cases
  $ cat >obj2.ml <<EOF
  > let a : < x : int >
  >   = _
  > let b : < x : int; .. >
  >   = _
  > type o = < x : int >
  > let x : < y: char; o >
  >   = _
  > EOF

  $ $MERLIN single construct -position 2:5 \
  > -filename obj2.ml <obj2.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

  $ $MERLIN single construct -position 4:5 \
  > -filename obj2.ml <obj2.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

  $ $MERLIN single construct -position 7:5 \
  > -filename obj2.ml <obj2.ml | jq ".value[1]"
  [
    "object method y = _ method x = _ end"
  ]
