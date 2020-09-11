  $ $MERLIN single type-enclosing -position 4:11 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 10
      },
      "end": {
        "line": 4,
        "col": 11
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 4,
        "col": 8
      },
      "end": {
        "line": 4,
        "col": 18
      },
      "type": "t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 6:11 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 6,
        "col": 8
      },
      "end": {
        "line": 6,
        "col": 18
      },
      "type": "'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:11 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 10
      },
      "end": {
        "line": 8,
        "col": 11
      },
      "type": "float",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 17
      },
      "type": "unit",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:9 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 9
      },
      "type": "t",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 9
      },
      "type": "t",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 8:9 -verbosity 1 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 9
      },
      "type": "type t = { mutable b : float; }",
      "tail": "no"
    },
    {
      "start": {
        "line": 8,
        "col": 8
      },
      "end": {
        "line": 8,
        "col": 9
      },
      "type": "type t = { mutable b : float; }",
      "tail": "no"
    }
  ]

FIXME: The following results are not entirely satisfying (`foo.Bar -> foo` could be expanded to `{ baz : unit } -> foo`)
  $ $MERLIN single type-enclosing -position 12:9 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 12,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 11
      },
      "type": "foo.Bar -> foo",
      "tail": "no"
    },
    {
      "start": {
        "line": 12,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 26
      },
      "type": "foo",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 12:9 -verbosity 1 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 12,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 11
      },
      "type": "foo.Bar -> foo",
      "tail": "no"
    },
    {
      "start": {
        "line": 12,
        "col": 8
      },
      "end": {
        "line": 12,
        "col": 26
      },
      "type": "type foo = Bar of { baz : unit; }",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 12:16 -verbosity 0 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 12,
        "col": 15
      },
      "end": {
        "line": 12,
        "col": 18
      },
      "type": "unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 12,
        "col": 12
      },
      "end": {
        "line": 12,
        "col": 26
      },
      "type": "foo.Bar",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 12:16 -verbosity 1 \
  > -filename ./record.ml < ./record.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 12,
        "col": 15
      },
      "end": {
        "line": 12,
        "col": 18
      },
      "type": "unit",
      "tail": "no"
    },
    {
      "start": {
        "line": 12,
        "col": 12
      },
      "end": {
        "line": 12,
        "col": 26
      },
      "type": "type Bar = { baz : unit; }",
      "tail": "no"
    }
  ]
