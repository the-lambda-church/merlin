(enabled_if (< %{ocaml_version} 4.06.0))

  $ $MERLIN single type-enclosing -position 5:11 -verbosity 0 \
  > -filename ./issue1109.ml < ./issue1109.ml | jq ".value[0:2]"
  [
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 14
      },
      "type": "'a -> 'a",
      "tail": "no"
    },
    {
      "start": {
        "line": 5,
        "col": 10
      },
      "end": {
        "line": 5,
        "col": 14
      },
      "type": "'a -> 'a",
      "tail": "no"
    }
  ]
