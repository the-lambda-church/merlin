From ocaml-lsp#444 (https://github.com/ocaml/ocaml-lsp/issues/444)
  $ cat >gadt.ml <<EOF
  > type 'a t =
  >   | A : [\`A] t
  >   | B : [\`B] t
  > 
  > let f x =
  >   match x with
  >   | A -> ()
  > EOF

  $ $MERLIN single type-enclosing -position 6:9 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0]"
  {
    "start": {
      "line": 6,
      "col": 8
    },
    "end": {
      "line": 6,
      "col": 9
    },
    "type": "[ `A ] t",
    "tail": "no"
  }

  $ $MERLIN single type-enclosing -position 7:5 -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value[0]"
  {
    "start": {
      "line": 7,
      "col": 4
    },
    "end": {
      "line": 7,
      "col": 5
    },
    "type": "[ `A ] t",
    "tail": "no"
  }


  $ $MERLIN single errors -verbosity 0 \
  > -filename ./gadt.ml < ./gadt.ml | tr '\r\n' ' ' | jq ".value"
  []
