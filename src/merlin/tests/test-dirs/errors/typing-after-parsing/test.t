First ask for all the errors:

  $ $MERLIN single errors -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 9
        },
        "end": {
          "line": 3,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type unit"
      },
      {
        "start": {
          "line": 7,
          "col": 9
        },
        "end": {
          "line": 7,
          "col": 10
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting expr"
      }
    ],
    "notifications": []
  }

Notice that the second type error is not returned, as it happens after the first
syntax error.

Now let's just ask for typing errors:

  $ $MERLIN single errors -lexing false -parsing false -filename test.ml < test.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 3,
          "col": 9
        },
        "end": {
          "line": 3,
          "col": 10
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type int but an expression was expected of type unit"
      }
    ],
    "notifications": []
  }

And let's also try filtering out type errors:

  $ $MERLIN single errors -typing false -filename test.ml < test.ml
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
          "col": 10
        },
        "type": "parser",
        "sub": [],
        "valid": true,
        "message": "Syntax error, expecting expr"
      }
    ],
    "notifications": []
  }
