Setup the test environment:

  $ $OCAMLC -c -bin-annot -o Foo_test test.ml
  $ $OCAMLC -c -bin-annot foo.ml
  $ $OCAMLC -c -bin-annot bar.ml

Do the thing:

  $ echo "let () = Bar.the_function ()" | \
  > $MERLIN single locate -look-for ml -position 1:15 -filename ./example.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/looping-substitution/test.ml",
      "pos": {
        "line": 5,
        "col": 4
      }
    },
    "notifications": []
  }
