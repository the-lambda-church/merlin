Get type of a shadowing let binding:

  $ $MERLIN single type-enclosing -position 4:4 -verbosity 0 \
  > -filename ./let.ml < ./let.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 4,
        "col": 4
      },
      "end": {
        "line": 4,
        "col": 7
      },
      "type": "float",
      "tail": "no"
    }
  ]
