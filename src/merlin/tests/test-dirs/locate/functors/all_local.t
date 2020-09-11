Check that we can jump locally inside the functor:

  $ $MERLIN single locate -look-for ml -position 14:12 -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/all_local.ml",
      "pos": {
        "line": 12,
        "col": 2
      }
    },
    "notifications": []
  }

Check that we can jump from inside the functor to the (sig of the) parameter:

  $ $MERLIN single locate -look-for ml -position 12:11 -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/all_local.ml",
      "pos": {
        "line": 9,
        "col": 12
      }
    },
    "notifications": []
  }

Check the argument is substituted for the parameter

  $ $MERLIN single locate -look-for ml -position 20:13 -filename ./all_local.ml < ./all_local.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/all_local.ml",
      "pos": {
        "line": 6,
        "col": 2
      }
    },
    "notifications": []
  }
