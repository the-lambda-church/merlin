FIXME: such substitutions are not handled properly yet.
On a similar note we currently have no way to decide between a sig and a struct
when both are present in the buffer (the struct will always be preferred).

  $ $MERLIN single locate -look-for ml -position 10:12 -filename ./basic.ml < ./basic.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/sig-substs/basic.ml",
      "pos": {
        "line": 3,
        "col": 4
      }
    },
    "notifications": []
  }
