(enabled_if (>= %{ocaml_version} 4.08.0))

Various parts of the letop:

- The operator:

  $ $MERLIN single type-enclosing -position 4:3 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "('a -> 'b) -> 'a option -> 'b option",
      "tail": "no"
    },
    {
      "start": {
        "line": 3,
        "col": 17
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "'a -> ('b -> 'c) -> 'b option -> 'c option",
      "tail": "no"
    }
  ]

- The pattern:

  $ $MERLIN single type-enclosing -position 4:8 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 7
      },
      "end": {
        "line": 4,
        "col": 10
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "('a -> 'b) -> 'a option -> 'b option",
      "tail": "no"
    }
  ]

- The rhs:

  $ $MERLIN single type-enclosing -position 4:22 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 13
      },
      "end": {
        "line": 4,
        "col": 29
      },
      "type": "('a, 'b) Hashtbl.t -> 'a -> 'b option",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 13
      },
      "end": {
        "line": 4,
        "col": 29
      },
      "type": "('a, 'b) Hashtbl.t -> 'a -> 'b option",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:31 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 30
      },
      "end": {
        "line": 4,
        "col": 33
      },
      "type": "('a, 'b) Hashtbl.t",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 30
      },
      "end": {
        "line": 4,
        "col": 33
      },
      "type": "('a, 'b) Hashtbl.t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 4:35 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 34
      },
      "end": {
        "line": 4,
        "col": 37
      },
      "type": "'a",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 34
      },
      "end": {
        "line": 4,
        "col": 37
      },
      "type": "'a",
      "tail": "no"
    }
  ]

- After the in:

  $ $MERLIN single type-enclosing -position 5:3 -verbosity 0 \
  > -filename ./letop.ml < ./letop.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 5
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 2
      },
      "end": {
        "line": 5,
        "col": 5
      },
      "type": "int",
      "tail": "no"
    }
  ]

