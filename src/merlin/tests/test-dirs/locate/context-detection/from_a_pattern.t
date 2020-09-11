Confirm there is a type error:

  $ $MERLIN single errors -filename ./from_a_pattern.ml < ./from_a_pattern.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 8,
          "col": 4
        },
        "end": {
          "line": 8,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound constructor Blah.Q"
      }
    ],
    "notifications": []
  }

We call locate from the broken pattern:

  $ $MERLIN single locate -look-for ml -position 8:7 -filename ./from_a_pattern.ml < ./from_a_pattern.ml
  {
    "class": "return",
    "value": {
      "file": "tests/test-dirs/locate/context-detection/from_a_pattern.ml",
      "pos": {
        "line": 1,
        "col": 0
      }
    },
    "notifications": []
  }
