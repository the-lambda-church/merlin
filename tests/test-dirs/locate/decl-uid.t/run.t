  $ touch .merlin

  $ $OCAMLC -c -bin-annot -no-keep-locs foo.mli

  $ $MERLIN single locate -look-for mli -position 1:14 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }

  $ $MERLIN single locate -look-for mli -position 2:15 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 4,
      "col": 2
    }
  }

  $ $MERLIN single locate -look-for mli -position 4:9 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/test.ml",
    "pos": {
      "line": 1,
      "col": 4
    }
  }

  $ $MERLIN single locate -look-for mli -position 6:12 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 1,
      "col": 0
    }
  }


  $ $MERLIN single locate -look-for mli -position 7:30 \
  > -filename foo.mli < foo.mli | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 3,
      "col": 0
    }
  }

  $ $MERLIN single locate -look-for mli -position 10:11 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 9,
      "col": 2
    }
  }

  $ $MERLIN single locate -look-for mli -position 11:11 \
  > -filename test.ml < test.ml | jq '.value'
  {
    "file": "$TESTCASE_ROOT/foo.mli",
    "pos": {
      "line": 7,
      "col": 41
    }
  }
