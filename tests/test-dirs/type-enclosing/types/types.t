TODO: these test are incorect !
should be solved with better context usage

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "x",
      "tail": "no"
    }
  ]

FIXME: not int but Foo

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 1 \
  > -filename ./types.ml < ./types.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "type x = Foo",
      "tail": "no"
    }
  ]
