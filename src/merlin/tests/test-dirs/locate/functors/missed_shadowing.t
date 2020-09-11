Reproduce bug described (and fixed) in commit e558d203334fd06f7653a6388b46dba895fb3ce9

  $ $MERLIN single locate -look-for ml -position 4:10 \
  > -filename ./missed_shadowing.ml < ./missed_shadowing.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/missed_shadowing.ml",
      "pos": {
        "line": 3,
        "col": 10
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:15 \
  > -filename ./missed_shadowing.ml < ./missed_shadowing.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/missed_shadowing.ml",
      "pos": {
        "line": 7,
        "col": 0
      }
    },
    "notifications": []
  }
