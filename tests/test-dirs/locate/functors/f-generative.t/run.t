Check that we handle generative functors properly:

  $ $MERLIN single locate -position 13:12 -look-for ml \
  > -filename generative.ml < generative.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/generative.ml",
      "pos": {
        "line": 8,
        "col": 6
      }
    },
    "notifications": []
  }

When -look-for is not set it defaults to MLI
  $ $MERLIN single locate -position 13:12 \
  > -filename generative.ml < generative.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/generative.ml",
      "pos": {
        "line": 3,
        "col": 2
      }
    },
    "notifications": []
  }
