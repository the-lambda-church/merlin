In the example, typing "None" will fail. 
Because the environment has constraints, the failure will happen between a
begin_def and an end_def.
Thus the error recovery will happen at the wrong level.
The fix is to save and restore levels when attempting a recoverable typing.

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 7,
          "col": 9
        },
        "end": {
          "line": 7,
          "col": 13
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type 'a option but an expression was expected of type
    a -> string"
      }
    ],
    "notifications": []
  }
