Searching foo from bar works:

  $ $MERLIN single locate -look-for ml -position 8:6 -filename ./issue973.ml < ./issue973.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/mutually-recursive/issue973.ml",
      "pos": {
        "line": 1,
        "col": 8
      }
    },
    "notifications": []
  }

And so does bar from foo:

  $ $MERLIN single locate -look-for ml -position 2:7 -filename ./issue973.ml < ./issue973.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/mutually-recursive/issue973.ml",
      "pos": {
        "line": 4,
        "col": 4
      }
    },
    "notifications": []
  }
