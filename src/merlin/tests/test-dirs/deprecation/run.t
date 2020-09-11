  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $OCAMLC -nopervasives -c -bin-annot foo.mli
  $ $MERLIN single complete-prefix -position 2:14 -prefix Foo.ba -kind val -filename x.ml < x.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "bar",
          "kind": "Value",
          "desc": "unit -> int",
          "info": "",
          "deprecated": true
        },
        {
          "name": "baz",
          "kind": "Value",
          "desc": "unit -> unit",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }
