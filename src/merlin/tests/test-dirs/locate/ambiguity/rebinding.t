Jumping to Z.foo before the rebinding of X:

  $ $MERLIN single locate -look-for ml -position 11:13 -filename ./rebinding.ml < ./rebinding.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/ambiguity/rebinding.ml",
      "pos": {
        "line": 4,
        "col": 10
      }
    },
    "notifications": []
  }

Jumping to Z.foo after the rebinding of X:

  $ $MERLIN single locate -look-for ml -position 15:13 -filename ./rebinding.ml < ./rebinding.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/ambiguity/rebinding.ml",
      "pos": {
        "line": 4,
        "col": 10
      }
    },
    "notifications": []
  }

