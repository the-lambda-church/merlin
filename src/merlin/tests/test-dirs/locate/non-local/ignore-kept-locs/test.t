Setup the test context:

  $ $OCAMLC -c -bin-annot -keep-locs a.ml

Make sure that we do not use locations coming from the cmi:

  $ $MERLIN single locate -look-for ml -log-section locate -log-file log \
  > -position 1:12 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/non-local/ignore-kept-locs/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

  $ grep -A1 Fallback log | grep -v Fallback
  [1]

  $ rm log

  $ $MERLIN single locate -look-for ml -log-section locate -log-file log \
  > -position 5:12 -filename ./b.ml < ./b.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/non-local/ignore-kept-locs/a.ml",
      "pos": {
        "line": 1,
        "col": 4
      }
    },
    "notifications": []
  }

The fallback here is ok, it points to the local buffer (to the include line
actually), not to a.ml

  $ grep -A1 Fallback log | grep -v Fallback
  File "b.ml", line 3, characters 0-9

  $ rm log
