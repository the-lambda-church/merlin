(enabled_if (>= %{ocaml_version} 4.08.0))

  $ $MERLIN single type-enclosing -position 1:5 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 1,
        "col": 4
      },
      "end": {
        "line": 1,
        "col": 5
      },
      "type": "< pop : int option; push : int -> unit >",
      "tail": "no"
    }
  ]

FIXME: not a very satisfying answer, we expect the type of the method
  $ $MERLIN single type-enclosing -position 11:10 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 1,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 3
      },
      "type": "< pop : int option; push : int -> unit >",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 14:5 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 14,
        "col": 4
      },
      "end": {
        "line": 14,
        "col": 5
      },
      "type": "type unit = ()",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 14:9 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 14,
        "col": 8
      },
      "end": {
        "line": 14,
        "col": 9
      },
      "type": "< pop : int option; push : int -> unit >",
      "tail": "no"
    },
    {
      "start": {
        "line": 14,
        "col": 8
      },
      "end": {
        "line": 14,
        "col": 9
      },
      "type": "< pop : int option; push : int -> unit >",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 14:11 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 14,
        "col": 8
      },
      "end": {
        "line": 14,
        "col": 14
      },
      "type": "int -> unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 14,
        "col": 8
      },
      "end": {
        "line": 14,
        "col": 16
      },
      "type": "type unit = ()",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 16:10 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 16,
        "col": 9
      },
      "end": {
        "line": 16,
        "col": 12
      },
      "type": "< pouet : string -> 'a; .. >",
      "tail": "no"
    },
    {
      "start": {
        "line": 16,
        "col": 9
      },
      "end": {
        "line": 16,
        "col": 28
      },
      "type": "< pouet : string -> 'a; .. > -> 'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 18:13 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 18,
        "col": 12
      },
      "end": {
        "line": 18,
        "col": 15
      },
      "type": "< pouet : string -> 'a >",
      "tail": "no"
    },
    {
      "start": {
        "line": 18,
        "col": 12
      },
      "end": {
        "line": 18,
        "col": 15
      },
      "type": "< pouet : string -> 'a >",
      "tail": "no"
    }
  ]

FIXME: same as before
  $ $MERLIN single type-enclosing -position 18:21 -verbosity 1 \
  > -filename ./test.ml < ./test.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 18,
        "col": 18
      },
      "end": {
        "line": 18,
        "col": 41
      },
      "type": "< pouet : string -> 'a >",
      "tail": "no"
    },
    {
      "start": {
        "line": 18,
        "col": 11
      },
      "end": {
        "line": 18,
        "col": 58
      },
      "type": "< pouet : string -> 'a > -> 'a",
      "tail": "no"
    }
  ]
