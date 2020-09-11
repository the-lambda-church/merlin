If warnings are not backtracked properly, when reusing the same type checking
environment in different queries, some warnings will be reported only once.

  $ $MERLIN server stop-server

  $ $MERLIN server errors -filename backtrack.ml -w +A <<EOF \
  > let f x = () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 7
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 27: unused variable x."
      }
    ],
    "notifications": []
  }

  $ $MERLIN server errors -filename backtrack.ml -w +A <<EOF \
  >  \
  > let f x = () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 6
        },
        "end": {
          "line": 2,
          "col": 7
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 27: unused variable x."
      }
    ],
    "notifications": []
  }

  $ $MERLIN server errors -filename backtrack.ml -w +A <<EOF \
  > let f x = () \
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 1,
          "col": 6
        },
        "end": {
          "line": 1,
          "col": 7
        },
        "type": "warning",
        "sub": [],
        "valid": true,
        "message": "Warning 27: unused variable x."
      }
    ],
    "notifications": []
  }

  $ $MERLIN server stop-server
