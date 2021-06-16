Regression test for #1228. Locting modules defined by alises

  $ cat > test.ml <<EOF
  > module M = struct end
  > module A = M
  > module K = A
  > EOF

  $ $MERLIN single locate -look-for ml -position 3:11 -filename ./test.ml < ./test.ml
  {
    "class": "return",
    "value": {
      "file": "$TESTCASE_ROOT/test.ml",
      "pos": {
        "line": 2,
        "col": 0
      }
    },
    "notifications": []
  }
