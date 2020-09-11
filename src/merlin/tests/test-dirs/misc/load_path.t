  $ echo "let x = List.map" >test.ml

Shadow the list module from the stdlib:

  $ echo "let map = 3" >list.ml
  $ $OCAMLC -c list.ml

Here is what the compiler sees:

  $ $OCAMLC -c -i test.ml
  val x : int

Here is what merlin sees:

  $ $MERLIN single type-enclosing -position 1:14 -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      },
      {
        "start": {
          "line": 1,
          "col": 8
        },
        "end": {
          "line": 1,
          "col": 16
        },
        "type": "int",
        "tail": "no"
      }
    ],
    "notifications": []
  }
