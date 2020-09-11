(enabled_if (< %{ocaml_version} 4.08.0))

  $ $MERLIN single locate -look-for ml -position 6:6 \
  > -filename ./environment_on_open.ml < ./environment_on_open.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/context-detection/environment_on_open.ml",
      "pos": {
        "line": 4,
        "col": 2
      }
    },
    "notifications": []
  }
