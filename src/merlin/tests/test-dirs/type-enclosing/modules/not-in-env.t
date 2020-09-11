  $ $MERLIN single type-enclosing -position 5:9 -verbosity 0 \
  > -filename ./not-in-env.ml < ./not-in-env.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 9
      },
      "type": "sig val y : int end",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./not-in-env.ml < ./not-in-env.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 8
      },
      "end": {
        "line": 5,
        "col": 11
      },
      "type": "'a",
      "tail": "no"
    }
  ]

  $ $MERLIN single type-enclosing -position 7:11 -verbosity 0 \
  > -filename ./not-in-env.ml < ./not-in-env.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 7,
        "col": 8
      },
      "end": {
        "line": 7,
        "col": 11
      },
      "type": "'a",
      "tail": "no"
    }
  ]
