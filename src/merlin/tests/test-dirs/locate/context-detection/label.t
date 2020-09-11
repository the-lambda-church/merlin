
  $ $MERLIN single locate -look-for ml -position 4:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/context-detection/label.ml",
      "pos": {
        "line": 3,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:11 -filename ./label.ml < ./label.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/context-detection/label.ml",
      "pos": {
        "line": 8,
        "col": 4
      }
    },
    "notifications": []
  }

