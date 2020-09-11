  $ $MERLIN single errors -pp cat -filename test.ml <<EOF \
  > let x : int = "hello" \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 14
        },
        "end": {
          "line": 1,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
  $ $MERLIN single errors -pp 'cpp -Wno-everything -E' -filename test.ml <<EOF \
  > #ifndef FOO \
  > let x : int = "hello" \
  > #else \
  > let x : int = 42 \
  > #endif \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 14
        },
        "end": {
          "line": 2,
          "col": 21
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "This expression has type string but an expression was expected of type int"
      }
    ],
    "notifications": []
  }
  $ $MERLIN single errors -pp 'cpp -Wno-everything -E' -filename test.ml <<EOF \
  > #ifdef FOO \
  > let x : int = "hello" \
  > #else \
  > let x : int = 42 \
  > #endif \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }
