When we change merlin configuration server should invalidate typing env
accordingly.

  $ $MERLIN server stop-server

First we query server with `./deps` in build path and we see no errors reported:

  $ $MERLIN server errors -filename consistent_env.ml -build-path ./deps <<EOF \
  > let f x = A.value \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

Then we query server with no `./deps` in build path and should get an error
about unbound module:

  $ $MERLIN server errors -filename consistent_env.ml <<EOF \
  > let f x = A.value \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 10
        },
        "end": {
          "line": 1,
          "col": 17
        },
        "type": "typer",
        "sub": [],
        "valid": true,
        "message": "Unbound module A"
      }
    ],
    "notifications": []
  }

Finally if we query server again with `./deps` in build path back we should see
no errors as in the first query:

  $ $MERLIN server errors -filename consistent_env.ml -build-path ./deps <<EOF \
  > let f x = A.value \
  > EOF
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN server stop-server
