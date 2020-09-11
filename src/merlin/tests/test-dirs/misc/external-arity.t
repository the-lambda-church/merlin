Bucklescript allow externals to have a non-arrow type. We relax this
restriction in Merlin too as it ease some peoples life and is not really
problematic for normal  OCaml users.

  $ $MERLIN single errors -filename external_arity.ml -w +A <<EOF \
  > external foo : unit list = "foo" \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
