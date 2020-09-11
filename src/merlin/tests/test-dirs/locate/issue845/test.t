To properly test this, we need to have the functor in a different file than the
one we jump from, otherwise we will get a location from the environment, which
can be used as a fallback something fails later on.

And we also do not want to use a functor from the stdlib, because locations,
paths, etc. will change between versions of OCaml, so we define and compile a
module containing a functor locally:

  $ $OCAMLC -c -bin-annot local_map.mli
  $ $OCAMLC -c -bin-annot local_map.ml

Test jumping to impl:

FIXME: this jumps to the .mli...

  $ $MERLIN single locate -look-for ml -position 1:24 -filename test.ml <<EOF \
  > module SM = Local_map.Make(String) \
  > EOF
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/issue845/local_map.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

Test jumping to intf:

  $ $MERLIN single locate -look-for mli -position 1:24 -filename test.ml <<EOF \
  > module SM = Local_map.Make(String) \
  > EOF
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/issue845/local_map.mli",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }

