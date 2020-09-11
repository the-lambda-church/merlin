We include another unit, compiled with -no-keep-locs, so there are no locations
in the environment to fallback to:

  $ $OCAMLC -c -bin-annot -no-keep-locs foo.ml

Test when the include is a name, this should directly redirect us to the right
thing.

  $ $MERLIN single locate -look-for mli -position 4:17 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/includes/foo.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

Test including a structure: there we will want to look up the ident again inside
the structure, but the stamp will have changed:

  $ $MERLIN single locate -look-for mli -position 10:17 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/includes/foo.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

