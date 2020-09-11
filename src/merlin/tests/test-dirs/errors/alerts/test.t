(enabled_if (>= %{ocaml_version} 4.08.0))

  $ echo "S .\nB .\nFLG -nopervasives" > .merlin
  $ $OCAMLC -nopervasives -c -bin-annot lib.mli
  $ $MERLIN single errors -filename main.ml < main.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 8
        },
        "end": {
          "line": 2,
          "col": 12
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Alert deprecated: Lib.sqrt
  I am deprecated"
      }
    ],
    "notifications": []
  }
