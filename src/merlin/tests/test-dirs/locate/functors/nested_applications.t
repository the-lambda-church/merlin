
  $ $MERLIN single locate -look-for ml -position 17:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/nested_applications.ml",
      "pos": {
        "line": 5,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 19:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 2
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 25:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/nested_applications.ml",
      "pos": {
        "line": 10,
        "col": 2
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 29:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/nested_applications.ml",
      "pos": {
        "line": 21,
        "col": 54
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 33:14 \
  > -filename ./nested_applications.ml < ./nested_applications.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/functors/nested_applications.ml",
      "pos": {
        "line": 5,
        "col": 0
      }
    },
    "notifications": []
  }

