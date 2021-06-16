Trying them all:

  $ $MERLIN single locate -look-for ml -position 5:9 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 7:17 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 3,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 9:12 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 7,
        "col": 0
      }
    },
    "notifications": []
  }

FIXME this should say "Already at definition point" (we're defining the label):

  $ $MERLIN single locate -look-for ml -position 13:12 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 5,
        "col": 4
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 13:16 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 16:12 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 5,
        "col": 4
      }
    },
    "notifications": []
  }

FIXME we failed to parse/reconstruct the ident, that's interesting

  $ $MERLIN single locate -look-for ml -position 16:16 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": "Not a valid identifier",
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 16:20 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 17:5 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 18:15 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 11,
        "col": 0
      }
    },
    "notifications": []
  }

FIXME this should jump to line 11:

  $ $MERLIN single locate -look-for ml -position 20:15 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 23:7 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": "Already at definition point",
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 23:13 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 13,
        "col": 0
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 24:3 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 23,
        "col": 6
      }
    },
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 24:5 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 13,
        "col": 0
      }
    },
    "notifications": []
  }
