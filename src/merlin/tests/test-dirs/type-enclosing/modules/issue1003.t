  $ $MERLIN single type-enclosing -position 5:14 -verbosity 0 \
  > -filename ./issue1003.ml < ./issue1003.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 16
      },
      "type": "int",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 16
      },
      "type": "int",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./issue1003.ml < ./issue1003.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 12
      },
      "type": "sig val foo : int end",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 16
      },
      "type": "int",
      "tail": "no"
    }
  ]
